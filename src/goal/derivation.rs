use super::*;
use pty::PtyMaster;
use std::collections::BTreeSet;

#[cfg(target_os = "linux")]
#[path = "derivation/linux.rs"]
mod sys;

#[cfg(target_os = "macos")]
#[path = "derivation/macos.rs"]
mod sys;

trait GoalImpl {
  fn setup_chroot(&mut self, store: &dyn Store) -> Result<()>;
  fn exec_child(
    &self,
    store: &dyn Store,
    read_side: PtyMaster,
    write_side: i32,
    sandbox_tmpdir: &PathBuf,
  ) -> Result<Child>;
}

trait WorkerImpl {
  fn init_chroot(&self, store: &dyn Store, derivation: &Derivation) -> Result<()>;
}

pub struct DerivationGoal {
  derivation: Derivation,
  drv_path: StorePath,
  build_user: Option<UserLock>,
  closure: BTreeSet<StorePath>,

  chroot_root: PathBuf,
  dirs_in_chroot: HashMap<String, ChrootDir>,
}

impl DerivationGoal {
  pub fn new(derivation: Derivation, drv_path: StorePath) -> Self {
    Self {
      derivation,
      drv_path,
      chroot_root: PathBuf::from("/no-such-path"),
      build_user: None,
      closure: BTreeSet::default(),
      dirs_in_chroot: HashMap::new(),
    }
  }

  pub fn local_build(&mut self, store: &dyn Store) -> Result<Child> {
    store.add_temp_root(&self.drv_path)?;

    if unistd::getuid().is_root() {
      if let Some(group) = &settings().build_users_group {
        if cfg!(unix) {
          let u = UserLock::get_free_user(group)?;
          u.kill()?;
          self.build_user = Some(u);
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
        store.print_store_path(&self.drv_path),
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
            store.print_store_path(&self.drv_path)
          );
        }
        if cfg!(target_os = "macos") && sandbox_profile.is_some() {
          bail!(
            "derivation '{}' specifies a sandbox profile, but that's only allowed when sandbox = \
             relaxed.",
            store.print_store_path(&self.drv_path)
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
        store.print_store_path(&output.path),
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

    let _env = self.init_env(store, &host_tmpdir, sandbox_tmpdir)?;

    if use_chroot {
      for chroot_dir in settings()
        .sandbox_paths
        .union(&settings().extra_sandbox_paths)
      {
        let (is_optional, chroot_dir) = match chroot_dir.strip_suffix('?') {
          Some(c) => (true, c),
          None => (false, chroot_dir.as_str()),
        };

        if let Some((k, v)) = break_str(chroot_dir, '=') {
          self.dirs_in_chroot.insert(
            k.into(),
            ChrootDir {
              path: v.into(),
              optional: is_optional,
            },
          );
        } else {
          self.dirs_in_chroot.insert(
            chroot_dir.into(),
            ChrootDir {
              path: chroot_dir.into(),
              optional: is_optional,
            },
          );
        }
      }

      self.dirs_in_chroot.insert(
        sandbox_tmpdir.display().to_string(),
        ChrootDir {
          path: host_tmpdir.clone(),
          optional: false,
        },
      );

      for dir in self.dirs_in_chroot.values() {
        if store.is_in_store(&dir.path) {
          store.compute_closure(
            &store.parse_store_path(&dir.path)?,
            &mut self.closure,
            false,
            false,
            false,
          )?;
        }
      }

      for item in &self.closure {
        let p = store.print_store_path(item);
        self.dirs_in_chroot.insert(
          p.clone(),
          ChrootDir {
            path: p.into(),
            optional: false,
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

        self.dirs_in_chroot.insert(
          path.into(),
          ChrootDir {
            path: path.into(),
            optional: false,
          },
        );
      }

      self.setup_chroot(store)?;

      if cfg!(not(unix)) {
        bail!("sandboxing builds is not supported on this platform.");
      }
    }

    debug!("executing builder `{}'", self.derivation.builder.display());

    let read_side = pty::posix_openpt(OFlag::O_RDWR | OFlag::O_NOCTTY)?;
    #[cfg(target_os = "linux")]
    let slave = pty::ptsname_r(&read_side)?;
    #[cfg(not(target_os = "linux"))]
    let slave = unsafe { pty::ptsname(&read_side)? };

    if let Some(u) = &self.build_user {
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

    if use_chroot {
      return self.exec_child(store, read_side, write_side, sandbox_tmpdir);
    }

    bail!("not implemented: building outside chroot")
  }

  fn init_env(
    &self,
    store: &dyn Store,
    host_tmpdir: &Path,
    sandbox_tmpdir: &Path,
  ) -> Result<HashMap<String, String>> {
    let mut env = HashMap::<String, String>::new();

    env.insert(String::from("PATH"), String::from("/path-not-set"));
    env.insert(String::from("HOME"), String::from("/homeless-shelter"));
    env.insert(
      String::from("NIX_STORE"),
      store.store_path().to_string_lossy().to_string(),
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

struct DerivationWorker<'a> {
  use_chroot: bool,
  chroot_root: PathBuf,
  sandbox_tmpdir: PathBuf,
  dirs_in_chroot: &'a HashMap<String, ChrootDir>,
  build_user: Option<&'a UserLock>,
  write_side: RawFd,
}

impl<'a> DerivationWorker<'a> {
  fn run(self, store: &dyn Store, derivation: &Derivation) -> Result<()> {
    let DerivationWorker {
      use_chroot,
      ref sandbox_tmpdir,
      write_side,
      ..
    } = self;
    unistd::setsid()?;
    unistd::dup2(write_side, std::io::stderr().as_raw_fd())?;
    unistd::dup2(std::io::stderr().as_raw_fd(), std::io::stdout().as_raw_fd())?;
    {
      let fd = fs::File::open("/dev/null")?;
      unistd::dup2(fd.as_raw_fd(), std::io::stdin().as_raw_fd())?;
    }

    if use_chroot {
      self.init_chroot(store, derivation)?;
    }

    if let Some(u) = self.build_user {
      // no nix::setgroups on macos because fuck you
      if unsafe { libc::setgroups(u.other_gids.len() as _, u.other_gids.as_ptr().cast()) == -1 } {
        bail!(std::io::Error::last_os_error());
      }
      unistd::setgid(u.gid)?;
      unistd::setuid(u.uid)?;
    }

    unistd::chdir(sandbox_tmpdir)
      .with_context(|| format!("trying to move into tmpdir `{}'", sandbox_tmpdir.display()))?;

    let mut cmd = std::process::Command::new(derivation.builder.as_os_str());

    cmd.arg0(
      derivation
        .builder
        .file_name()
        .expect("builder should always have a filename"),
    );
    cmd.args(derivation.args.iter());
    println!("{:?}", &cmd);

    eprintln!("\x01");

    bail!(cmd.exec())
  }
}

fn tmp_build_dir(
  root: Option<&Path>,
  prefix: impl AsRef<Path>,
  include_pid: bool,
  use_global_counter: bool,
  mode: Mode,
) -> Result<AutoDelete> {
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
        fs::set_permissions(&tmpdir, fs::Permissions::from_mode(mode.bits() as _))?;
        if cfg!(target_os = "freebsd") {
          unistd::chown(&tmpdir, None, Some(unistd::getegid()))?;
        }
        return Ok(AutoDelete(tmpdir));
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
