use crate::{prelude::*, sync::user_lock::UserLock};
use settings::SandboxMode;
use std::{
  collections::{HashMap, HashSet},
  io::{BufRead, ErrorKind::AlreadyExists, Write},
  os::unix::{
    fs::PermissionsExt,
    io::{AsRawFd, FromRawFd},
  },
  sync::atomic::{AtomicUsize, Ordering},
};
use unix::{
  fcntl::OFlag,
  pty, sched,
  sys::{mman, stat::Mode, termios, wait},
  unistd,
};

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum ExitCode {
  Busy,
  Success,
  Failure,
  NoSubstituters,
  IncompleteClosure,
}

pub trait Goal {
  fn status(&self) -> ExitCode;
  fn key(&self) -> String;
}

pub struct DerivationGoal<'a> {
  derivation: Derivation,
  store: &'a dyn Store,
  drv_path: &'a StorePath,
}

impl<'a> DerivationGoal<'a> {
  const SANDBOX_GID: u32 = 100;
  const SANDBOX_UID: u32 = 1000;

  pub fn local_build(&self) -> Result<()> {
    #[cfg(unix)]
    let mut build_user: Option<UserLock> = None;

    if unistd::getuid().is_root() {
      if let Some(_group) = &settings().build_users_group {
        if cfg!(unix) {
          let u = UserLock::get_free_user()?;
          u.kill()?;
          build_user = Some(u);
        } else {
          bail!("build users are not supported on this platform")
        }
      }
    }

    if !self.derivation.can_build_locally() {
      use itertools::Itertools;
      bail!(
        "a '{}' with features {{{}}} is required to build '{}', but I am a '{}' with features \
         {{{}}}",
        self.derivation.platform,
        self.derivation.required_system_features().iter().join(", "),
        self.store.print_store_path(self.drv_path),
        settings().this_system,
        settings().system_features.iter().join(", ")
      )
    }

    let sandbox_profile = if cfg!(target_os = "macos") {
      self.derivation.env.get("__sandboxProfile")
    } else {
      None
    };

    let no_chroot = self.derivation.env.get("__noChroot") == Some(&String::from("1"));
    let use_chroot = match settings().sandbox_mode {
      SandboxMode::Enabled => {
        if no_chroot {
          bail!(
            "derivation '{}' has `__noChroot' set, but that's not allowed when sandbox = true.",
            self.store.print_store_path(self.drv_path)
          );
        }
        if cfg!(target_os = "macos") && sandbox_profile.is_some() {
          bail!(
            "derivation '{}' specifies a sandbox profile, but that's only allowed when sandbox = \
             relaxed.",
            self.store.print_store_path(self.drv_path)
          );
        }
        true
      }
      SandboxMode::Disabled => false,
      SandboxMode::Relaxed => !no_chroot,
    };

    let host_tmpdir = tmp_build_dir(
      None,
      format!("nix-build-{}", self.drv_path.name),
      false,
      false,
      unsafe { Mode::from_bits_unchecked(0o700) },
    )?;

    let mut input_rewrites = HashMap::new();

    for (key, output) in &self.derivation.outputs {
      input_rewrites.insert(
        crate::derivation::hash_placeholder(key),
        self.store.print_store_path(&output.path),
      );
    }

    #[cfg(target_os = "linux")]
    let sandbox_tmpdir = if use_chroot {
      &settings().sandbox_build_dir
    } else {
      &host_tmpdir
    };
    #[cfg(not(target_os = "linux"))]
    let sandbox_tmpdir = &host_tmpdir;

    let env = self.init_env(&host_tmpdir, sandbox_tmpdir)?;

    if use_chroot {
      #[derive(Debug)]
      struct ChrootDir {
        path: PathBuf,
        is_optional: bool,
      }

      let mut dirs_in_chroot = HashMap::<String, ChrootDir>::new();

      for chroot_dir in settings()
        .sandbox_paths
        .union(&settings().extra_sandbox_paths)
      {
        let (is_optional, chroot_dir) = match chroot_dir.strip_suffix('?') {
          Some(c) => (true, c),
          None => (false, chroot_dir.as_str()),
        };

        if let Some((k, v)) = break_str(chroot_dir, '=') {
          dirs_in_chroot.insert(
            k.into(),
            ChrootDir {
              path: v.into(),
              is_optional,
            },
          );
        } else {
          dirs_in_chroot.insert(
            chroot_dir.into(),
            ChrootDir {
              path: chroot_dir.into(),
              is_optional,
            },
          );
        }
      }

      dirs_in_chroot.insert(
        sandbox_tmpdir.display().to_string(),
        ChrootDir {
          path: host_tmpdir.clone(),
          is_optional: false,
        },
      );

      let mut closure = Default::default();
      for dir in dirs_in_chroot.values() {
        if self.store.is_in_store(&dir.path) {
          self
            .store
            .compute_closure(&self.store.parse_store_path(&dir.path)?, &mut closure)?;
        }
      }

      for item in closure {
        let p = self.store.print_store_path(&item);
        dirs_in_chroot.insert(
          p.clone(),
          ChrootDir {
            path: p.into(),
            is_optional: false,
          },
        );
      }

      let allowed_paths = &settings().allowed_impure_host_prefixes;

      let requested_paths = self
        .derivation
        .env
        .get("__impureHostDeps")
        .map_or(Default::default(), |x| {
          x.split_ascii_whitespace().collect::<Vec<_>>()
        });

      for path in requested_paths {
        if allowed_paths
          .iter()
          .all(|x| !Path::new(path).starts_with(x))
        {
          bail!(
            "derivation requested host path `{}', but it was not in allowed-impure-host-deps",
            path
          );
        }

        dirs_in_chroot.insert(
          path.into(),
          ChrootDir {
            path: path.into(),
            is_optional: false,
          },
        );
      }

      #[cfg(target_os = "linux")]
      {
        let chroot_root = self
          .store
          .to_real_path(self.drv_path)?
          .with_extension("chroot");
        let _ = fs::remove_dir(&chroot_root);

        debug!(
          "setting up chroot environment in `{}'",
          chroot_root.display()
        );

        fs::create_dir(&chroot_root)?;
        fs::set_permissions(&chroot_root, fs::Permissions::from_mode(0o750))?;

        if let Some(u) = &build_user {
          unistd::chown(&chroot_root, None, Some(u.gid)).with_context(|| {
            anyhow!(
              "while trying to chown dir `{}' to build-user `{}'",
              chroot_root.display(),
              u.uid
            )
          })?;
        }

        let chroot_tmp = chroot_root.join("tmp");
        fs::create_dir(&chroot_tmp)?;
        fs::set_permissions(&chroot_tmp, fs::Permissions::from_mode(0o1777))?;

        fs::create_dir(chroot_root.join("etc"))?;
        fs::write(
          chroot_root.join("etc").join("passwd"),
          format!(
            "root:x:0:0:Nix build user:{2}:/noshell\nnixbld:x:{0}:{1}:Nix build \
             user:{2}:/noshell\nnobody:x:65534:65534:Nobody:/:/noshell\n",
            Self::SANDBOX_UID,
            Self::SANDBOX_GID,
            settings().sandbox_build_dir.display()
          ),
        )?;

        fs::write(
          chroot_root.join("etc").join("group"),
          format!(
            "root:x:0:\nnixbld:!:{}:\nnogroup:x:65534:\n",
            Self::SANDBOX_GID
          ),
        )?;

        fs::write(
          chroot_root.join("etc").join("hosts"),
          "127.0.0.1 localhost\n::1 localhost\n",
        )?;

        let chroot_store_dir = chroot_root.join(self.store.store_path());
        fs::create_dir_all(&chroot_store_dir)?;
        fs::set_permissions(&chroot_store_dir, fs::Permissions::from_mode(0o1775))?;

        if let Some(u) = &build_user {
          unistd::chown(&chroot_store_dir, None, Some(u.gid)).with_context(|| {
            anyhow!(
              "while trying to chown store dir `{}' to build-user `{}'",
              chroot_store_dir.display(),
              u.uid
            )
          })?;
        }

        for output in self.derivation.outputs.values() {
          dirs_in_chroot.remove(&self.store.print_store_path(&output.path));
        }
      }

      if cfg!(not(unix)) {
        bail!("sandboxing builds is not supported on this platform.");
      }
    }

    debug!("executing builder `{}'", self.derivation.builder.display());

    let read_side = pty::posix_openpt(OFlag::O_RDWR | OFlag::O_NOCTTY)?;
    let slave = pty::ptsname_r(&read_side)?;

    if let Some(u) = &build_user {
      fs::set_permissions(&slave, fs::Permissions::from_mode(0o600))?;
      unistd::chown(Path::new(&slave), Some(u.uid), None)?;
    } else {
      #[cfg(target_os = "macos")]
      pty::grantpt(&read_side)?;
    }

    pty::unlockpt(&read_side)?;

    let write_side = unix::fcntl::open(
      Path::new(&slave),
      OFlag::O_RDWR | OFlag::O_NOCTTY,
      Mode::empty(),
    )?;

    let mut term = termios::tcgetattr(write_side)?;
    termios::cfmakeraw(&mut term);
    termios::tcsetattr(write_side, termios::SetArg::TCSANOW, &term)?;

    #[cfg(target_os = "linux")]
    if use_chroot {
      match unistd::fork()? {
        unistd::ForkResult::Parent { child } => match wait::waitpid(child, None)? {
          wait::WaitStatus::Exited(_, 0) => {
            let mut reader =
              std::io::BufReader::new(unsafe { fs::File::from_raw_fd(read_side.as_raw_fd()) })
                .lines();
            let child_pid = if let Some(line) = reader.next() {
              line?.parse::<libc::pid_t>()?
            } else {
              bail!("no output from child")
            };

            let host_uid = build_user.as_ref().map_or(unistd::getuid(), |x| x.uid);
            let host_gid = build_user.as_ref().map_or(unistd::getgid(), |x| x.gid);

            fs::write(
              format!("/proc/{}/uid_map", child_pid),
              format!("{} {} 1", Self::SANDBOX_UID, host_uid),
            )?;

            fs::write(format!("/proc/{}/setgroups", child_pid), "deny")?;

            fs::write(
              format!("/proc/{}/gid_map", child_pid),
              format!("{} {} 1", Self::SANDBOX_GID, host_gid),
            )?;

            fs::OpenOptions::new()
              .read(true)
              .write(false)
              .open(format!("/proc/{}/ns/mnt", child_pid))?;

            for builder_line in reader {
              let builder_line = builder_line?;
              match builder_line.strip_prefix('\x01') {
                Some(y) => {
                  if y.is_empty() {
                    break;
                  } else {
                    bail!("{}", y)
                  }
                }
                None => debug!("{}", builder_line),
              }
            }
          }
          x => bail!("unexpected wait status from child: {:?}", x),
        },
        unistd::ForkResult::Child => {
          if unistd::getuid().is_root() {
            unistd::setgroups(&[])?;
          }

          let stack_size = 1024 * 1024;
          let stack = unsafe {
            mman::mmap(
              std::ptr::null_mut(),
              stack_size,
              mman::ProtFlags::PROT_WRITE | mman::ProtFlags::PROT_READ,
              mman::MapFlags::MAP_PRIVATE
                | mman::MapFlags::MAP_ANONYMOUS
                | mman::MapFlags::MAP_STACK,
              -1,
              0,
            )?
            .cast::<u8>()
          };
          let stack = unsafe { std::slice::from_raw_parts_mut(stack, stack_size) };
          let child = sched::clone(
            Box::new(move || match self.run_child() {
              Ok(()) => 0,
              Err(e) => {
                eprintln!("{:?}", e);
                1
              }
            }),
            stack,
            sched::CloneFlags::CLONE_NEWUSER
              | sched::CloneFlags::CLONE_NEWPID
              | sched::CloneFlags::CLONE_NEWNS
              | sched::CloneFlags::CLONE_NEWIPC
              | sched::CloneFlags::CLONE_NEWUTS
              | sched::CloneFlags::CLONE_PARENT,
            Some(libc::SIGCHLD),
          )?;

          unsafe { fs::File::from_raw_fd(write_side) }
            .write_all(format!("{}\n", child).as_bytes())?;
        }
      }
    }

    Ok(())
  }

  fn run_child(&self) -> Result<()> {
    debug!("hello from the chilfd builder!");
    Ok(())
  }

  fn init_env(&self, host_tmpdir: &Path, sandbox_tmpdir: &Path) -> Result<HashMap<String, String>> {
    let mut env = HashMap::<String, String>::new();

    env.insert(String::from("PATH"), String::from("/path-not-set"));
    env.insert(String::from("HOME"), String::from("/homeless-shelter"));
    env.insert(
      String::from("NIX_STORE"),
      self.store.store_path().to_string_lossy().to_string(),
    );
    env.insert(
      String::from("NIX_BUILD_CORES"),
      format!("{}", settings().build_cores),
    );
    env.insert(String::from("NIX_LOG_FD"), String::from("2"));
    env.insert(String::from("TERM"), String::from("xterm-256color"));

    let pass_as_file: HashSet<_> = self
      .derivation
      .env
      .get("passAsfile")
      .map_or(Default::default(), |x| x.split_ascii_whitespace().collect());

    for (k, v) in &self.derivation.env {
      if pass_as_file.contains(k.as_str()) {
        let attr_hash = Hash::hash_str(k, HashType::SHA256);
        let filename = format!(".attr-{}", attr_hash.encode(Encoding::Base32));
        let filepath = host_tmpdir.join(&filename);
        std::fs::write(&filepath, v.as_bytes())?;
        env.insert(
          format!("{}Path", k),
          sandbox_tmpdir.join(filename).to_string_lossy().to_string(),
        );
      } else {
        env.insert(k.to_string(), v.to_string());
      }
    }

    for alias in &["NIX_BUILD_TOP", "TMPDIR", "TEMPDIR", "TMP", "TEMP", "PWD"] {
      env.insert(String::from(*alias), sandbox_tmpdir.display().to_string());
    }

    Ok(env)
  }
}

fn tmp_build_dir(
  root: Option<&Path>,
  prefix: impl AsRef<Path>,
  include_pid: bool,
  use_global_counter: bool,
  mode: Mode,
) -> Result<PathBuf> {
  lazy_static! {
    static ref GLOBAL_COUNTER: AtomicUsize = AtomicUsize::new(0);
  }

  let prefix = prefix.as_ref();
  let cnt = AtomicUsize::new(0);

  let counter = if use_global_counter {
    &*GLOBAL_COUNTER
  } else {
    &cnt
  };

  loop {
    let tmpdir = tempname(root, prefix, include_pid, counter)?;
    match fs::create_dir(&tmpdir) {
      Ok(()) => {
        let mut perms = fs::metadata(&tmpdir)?.permissions();
        perms.set_mode(mode.bits());
        fs::set_permissions(&tmpdir, perms)?;
        if cfg!(target_os = "freebsd") {
          unistd::chown(&tmpdir, None, Some(unistd::getegid()))?;
        }
        return Ok(tmpdir);
      }
      Err(e) => {
        if e.kind() != AlreadyExists {
          return Err(e).with_context(|| {
            format!(
              "while creating temporary build directory {}",
              tmpdir.display()
            )
          });
        }
      }
    }
  }
}

fn tempname(
  root: Option<&Path>,
  prefix: impl AsRef<Path>,
  include_pid: bool,
  counter: &AtomicUsize,
) -> Result<PathBuf> {
  let tmproot = root
    .map_or_else(
      || PathBuf::from(std::env::var("TMPDIR").unwrap_or_else(|_| String::from("/tmp"))),
      |x| x.to_path_buf(),
    )
    .canonicalize()?;
  Ok(if include_pid {
    tmproot.join(format!(
      "{}-{}-{}",
      prefix.as_ref().display(),
      std::process::id(),
      counter.fetch_add(1, Ordering::Acquire)
    ))
  } else {
    tmproot.join(format!(
      "{}-{}",
      prefix.as_ref().display(),
      counter.fetch_add(1, Ordering::Acquire)
    ))
  })
}
