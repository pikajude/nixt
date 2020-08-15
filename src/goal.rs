use crate::{prelude::*, sync::user_lock::UserLock};
use linux_personality::Personality;
use settings::SandboxMode;
use std::collections::{HashMap, HashSet};
use std::io::{BufRead, ErrorKind::AlreadyExists, Write};
use std::os::unix::{
  fs::{symlink, PermissionsExt},
  io::{AsRawFd, FromRawFd},
  prelude::RawFd,
  process::CommandExt,
};
use std::sync::{
  atomic::{AtomicUsize, Ordering},
  Arc,
};
use std::time::Instant;

use unix::{
  fcntl::OFlag,
  mount, pty, sched,
  sys::{mman, socket, stat::Mode, termios, wait},
  unistd,
};

fn personality(p: Personality) -> Result<Personality> {
  Ok(linux_personality::personality(p).map_err(|_| std::io::Error::last_os_error())?)
}

fn get_personality() -> Result<Personality> {
  Ok(linux_personality::get_personality().map_err(|_| std::io::Error::last_os_error())?)
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum ExitCode {
  Busy,
  Success,
  Failure,
  NoSubstituters,
  IncompleteClosure,
}

pub struct Worker {
  pub store: Arc<dyn Store>,
  pub goals: Vec<Goal>,
}

impl Worker {
  pub fn new(store: Arc<dyn Store>) -> Self {
    Self {
      store,
      goals: vec![],
    }
  }

  pub fn run(self) -> Result<()> {
    let mut fds = vec![];
    for goal in self.goals {
      let next_store = Arc::clone(&self.store);
      match goal {
        Goal::Derivation(d) => {
          let handle = std::thread::spawn(move || {
            let child = d.local_build(next_store.as_ref())?;
            debug!("starting child: {:?}", child.start_time);
            for line in child.output {
              let line = line?;
              if line.starts_with('\x01') {
                bail!("{}", &line[1..]);
              } else {
                info!("{}", line);
              }
            }
            Ok(())
          });
          fds.push(handle);
        }
      }
    }

    fds.into_iter().try_for_each(|x| match x.join() {
      Err(e) => {
        if let Some(msg) = e.downcast_ref::<String>() {
          bail!("child failed: {}", msg)
        } else {
          bail!("child panicked with an unprintable message")
        }
      }
      Ok(x) => x,
    })
  }
}

pub enum Goal {
  Derivation(DerivationGoal),
}

type ChildOutput = io::Lines<io::BufReader<fs::File>>;

#[derive(Debug)]
pub struct Child {
  output: ChildOutput,
  pid: unistd::Pid,
  start_time: Instant,
}

impl Child {
  pub fn new(output: io::Lines<io::BufReader<fs::File>>, pid: unistd::Pid) -> Self {
    Self {
      output,
      pid,
      start_time: Instant::now(),
    }
  }
}

#[derive(Debug)]
struct ChrootDir {
  path: PathBuf,
  optional: bool,
}

pub struct DerivationGoal {
  pub derivation: Derivation,
  pub drv_path: StorePath,
}

const SANDBOX_GID: u32 = 100;
const SANDBOX_UID: u32 = 1000;

impl DerivationGoal {
  pub fn local_build(&self, store: &dyn Store) -> Result<Child> {
    #[cfg(unix)]
    let mut build_user: Option<UserLock> = None;

    if unistd::getuid().is_root() {
      if let Some(group) = &settings().build_users_group {
        if cfg!(unix) {
          let u = UserLock::get_free_user(group)?;
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

    let mut chroot_root = PathBuf::from("/no-such-path");
    let mut dirs_in_chroot = HashMap::<String, ChrootDir>::new();

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
          dirs_in_chroot.insert(
            k.into(),
            ChrootDir {
              path: v.into(),
              optional: is_optional,
            },
          );
        } else {
          dirs_in_chroot.insert(
            chroot_dir.into(),
            ChrootDir {
              path: chroot_dir.into(),
              optional: is_optional,
            },
          );
        }
      }

      dirs_in_chroot.insert(
        sandbox_tmpdir.display().to_string(),
        ChrootDir {
          path: host_tmpdir.clone(),
          optional: false,
        },
      );

      let mut closure = Default::default();
      for dir in dirs_in_chroot.values() {
        if store.is_in_store(&dir.path) {
          store.compute_closure(
            &store.parse_store_path(&dir.path)?,
            &mut closure,
            false,
            false,
            false,
          )?;
        }
      }

      for item in &closure {
        let p = store.print_store_path(item);
        dirs_in_chroot.insert(
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

        dirs_in_chroot.insert(
          path.into(),
          ChrootDir {
            path: path.into(),
            optional: false,
          },
        );
      }

      #[cfg(target_os = "linux")]
      {
        chroot_root = store.to_real_path(&self.drv_path)?.with_extension("chroot");

        debug!(
          "setting up chroot environment in `{}'",
          chroot_root.display()
        );

        if let Err(e) = fs::remove_dir_all(&chroot_root) {
          if e.kind() != io::ErrorKind::NotFound {
            bail!(e)
          }
        }
        fs::create_dir_all(&chroot_root)?;
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
        fs::create_dir_all(&chroot_tmp)?;
        fs::set_permissions(&chroot_tmp, fs::Permissions::from_mode(0o1777))?;

        fs::create_dir_all(chroot_root.join("etc"))?;
        fs::write(
          chroot_root.join("etc").join("passwd"),
          format!(
            "root:x:0:0:Nix build user:{2}:/noshell\nnixbld:x:{0}:{1}:Nix build \
             user:{2}:/noshell\nnobody:x:65534:65534:Nobody:/:/noshell\n",
            SANDBOX_UID,
            SANDBOX_GID,
            settings().sandbox_build_dir.display()
          ),
        )?;

        fs::write(
          chroot_root.join("etc").join("group"),
          format!("root:x:0:\nnixbld:!:{}:\nnogroup:x:65534:\n", SANDBOX_GID),
        )?;

        fs::write(
          chroot_root.join("etc").join("hosts"),
          "127.0.0.1 localhost\n::1 localhost\n",
        )?;

        let chroot_store_dir = chroot_root.join(
          AsRef::<Path>::as_ref(&store.store_path())
            .strip_prefix("/")
            .expect("nix store path should be absolute"),
        );
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

        for (path, output_set) in &self.derivation.input_derivations {
          ensure!(
            store.is_valid_path(path)?,
            "input derivation {} is not valid",
            store.print_store_path(&path)
          );

          let drv =
            store.parse_derivation(&store.to_real_path(path)?, path.name.to_string().as_str())?;

          for output in output_set {
            let path = drv
              .outputs
              .get(output)
              .ok_or_else(|| anyhow!("derivation requires non-existent output name"))?;
            store.compute_closure(&path.path, &mut closure, false, false, false)?;
          }
        }

        for path in &self.derivation.input_sources {
          store.compute_closure(path, &mut closure, false, false, false)?;
        }

        debug!("derivation requires input paths: {:?}", &closure);

        for output in self.derivation.outputs.values() {
          dirs_in_chroot.remove(&store.print_store_path(&output.path));
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
              unistd::Pid::from_raw(line?.parse::<libc::pid_t>()?)
            } else {
              bail!("no output from child")
            };

            let host_uid = build_user.as_ref().map_or(unistd::getuid(), |x| x.uid);
            let host_gid = build_user.as_ref().map_or(unistd::getgid(), |x| x.gid);

            fs::write(
              format!("/proc/{}/uid_map", child_pid),
              format!("{} {} 1", SANDBOX_UID, host_uid),
            )?;

            fs::write(format!("/proc/{}/setgroups", child_pid), "deny")?;

            fs::write(
              format!("/proc/{}/gid_map", child_pid),
              format!("{} {} 1", SANDBOX_GID, host_gid),
            )?;

            fs::OpenOptions::new()
              .read(true)
              .write(false)
              .open(format!("/proc/{}/ns/mnt", child_pid))?;

            while let Some(builder_line) = reader.next() {
              let builder_line = builder_line?;
              match builder_line.strip_prefix('\x01') {
                Some(y) => {
                  if y.is_empty() {
                    break;
                  } else {
                    bail!("{}", y)
                  }
                }
                None => info!("{}", builder_line),
              }
            }

            // fd will be closed by BufReader drop
            std::mem::forget(read_side);

            return Ok(Child::new(reader, child_pid));
          }
          x => bail!("unexpected wait status from child: {:?}", x),
        },
        unistd::ForkResult::Child => {
          #[cfg(target_os = "linux")]
          {
            if unsafe { libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL) } == -1 {
              bail!(std::io::Error::last_os_error());
            }
          }

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
            Box::new(move || {
              match (DerivationWorker {
                use_chroot,
                chroot_root: chroot_root.clone(),
                sandbox_tmpdir: sandbox_tmpdir.to_path_buf(),
                dirs_in_chroot: &dirs_in_chroot,
                build_user: build_user.as_ref(),
                write_side,
              }
              .run(store, &self.derivation))
              {
                Ok(()) => 0,
                Err(e) => {
                  println!("{:?}", e);
                  eprintln!("\x01while setting up the build environment: {}", e);
                  1
                }
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
      chroot_root,
      sandbox_tmpdir,
      dirs_in_chroot,
      build_user,
      write_side,
    } = self;
    unistd::setsid()?;
    unistd::dup2(write_side, std::io::stderr().as_raw_fd())?;
    unistd::dup2(std::io::stderr().as_raw_fd(), std::io::stdout().as_raw_fd())?;
    {
      let fd = fs::File::open("/dev/null")?;
      unistd::dup2(fd.as_raw_fd(), std::io::stdin().as_raw_fd())?;
    }

    #[cfg(target_os = "linux")]
    if use_chroot {
      let sock = socket::socket(
        socket::AddressFamily::Inet,
        socket::SockType::Datagram,
        socket::SockFlag::empty(),
        None,
      )?;

      if false {
        netdevice::set_flags(
          sock,
          "lo",
          &(netdevice::IFF_UP | netdevice::IFF_LOOPBACK | netdevice::IFF_RUNNING),
        )?;
      }

      unistd::sethostname("localhost")?;
      if unsafe { libc::setdomainname(b"(none)".as_ptr().cast(), 4) } == -1 {
        bail!(std::io::Error::last_os_error());
      }

      let null: Option<&str> = None;

      mount::mount(
        null,
        "/",
        null,
        mount::MsFlags::MS_PRIVATE | mount::MsFlags::MS_REC,
        null,
      )?;

      mount::mount(
        Some(chroot_root.as_path()),
        chroot_root.as_path(),
        null,
        mount::MsFlags::MS_BIND,
        null,
      )?;

      let chroot_store = chroot_root.join(
        AsRef::<Path>::as_ref(&store.store_path())
          .strip_prefix("/")
          .expect("nix store path should be absolute"),
      );
      mount::mount(
        Some(chroot_store.as_path()),
        chroot_store.as_path(),
        null,
        mount::MsFlags::MS_BIND,
        null,
      )?;

      mount::mount(
        null,
        chroot_store.as_path(),
        null,
        mount::MsFlags::MS_SHARED,
        null,
      )?;

      for (target, dir) in dirs_in_chroot {
        let source = dir.path.as_path();
        if source.to_str() == Some("/proc") {
          continue;
        }
        bind(
          source,
          chroot_root.join(Path::new(&target).strip_prefix("/").unwrap()),
          dir.optional,
        )?;
      }

      fs::create_dir_all(chroot_root.join("dev").join("shm"))?;
      fs::create_dir_all(chroot_root.join("dev").join("pts"))?;
      if settings().system_features.contains("kvm") {
        fs::create_dir_all(chroot_root.join("dev").join("kvm"))?;
      }
      symlink("/proc/self/fd", chroot_root.join("dev").join("fd"))?;
      symlink("/proc/self/fd/0", chroot_root.join("dev").join("stdin"))?;
      symlink("/proc/self/fd/1", chroot_root.join("dev").join("stdout"))?;
      symlink("/proc/self/fd/2", chroot_root.join("dev").join("stderr"))?;

      fs::create_dir_all(chroot_root.join("proc"))?;
      mount::mount(
        Some("none"),
        &chroot_root.join("proc"),
        Some("proc"),
        mount::MsFlags::empty(),
        null,
      )?;

      if fs::metadata("/dev/shm").is_ok() {
        mount::mount(
          Some("none"),
          &chroot_root.join("dev").join("shm"),
          Some("tmpfs"),
          mount::MsFlags::empty(),
          Some(format!("size={}", settings().sandbox_shm_size).as_str()),
        )?;
      }

      if fs::metadata("/dev/pts/ptmx").is_ok() {
        mount::mount(
          Some("none"),
          &chroot_root.join("dev").join("pts"),
          Some("devpts"),
          mount::MsFlags::empty(),
          Some("newinstance,mode=0620"),
        )?;
        symlink("/dev/pts/ptmx", chroot_root.join("dev").join("ptmx"))?;
        fs::set_permissions(
          chroot_root.join("dev").join("pts").join("ptmx"),
          fs::Permissions::from_mode(0o666),
        )?;
      }

      sched::unshare(sched::CloneFlags::CLONE_NEWNS)?;
      unistd::chdir(chroot_root.as_path())?;
      fs::create_dir_all("real-root")?;
      unistd::pivot_root(".", "real-root")?;
      unistd::chroot(".")?;
      mount::umount2("real-root", mount::MntFlags::MNT_DETACH)?;
      fs::remove_dir("real-root")?;
      unistd::setgid(unistd::Gid::from_raw(SANDBOX_GID))?;
      unistd::setuid(unistd::Uid::from_raw(SANDBOX_UID))?;
    }

    unistd::chdir(&sandbox_tmpdir)
      .with_context(|| format!("trying to move into tmpdir `{}'", sandbox_tmpdir.display()))?;

    let uname = unix::sys::utsname::uname();
    if derivation.platform == "i686-linux"
      && (settings().this_system == "x86_64-linux"
        || (uname.sysname() == "Linux" && uname.machine() == "x86_64"))
    {
      personality(linux_personality::PER_LINUX32)?;
    }

    if derivation.platform == "i686-linux"
      || derivation.platform == "x86_64-linux" && settings().impersonate_linux_26
    {
      personality(get_personality()? | linux_personality::UNAME26)?;
    }

    personality(get_personality()? | linux_personality::ADDR_NO_RANDOMIZE)?;

    rlimit::setrlimit(rlimit::Resource::CORE, 0, rlimit::RLIM_INFINITY)?;

    if let Some(u) = build_user {
      unistd::setgroups(&u.other_gids)?;
      unistd::setgid(u.gid)?;
      unistd::setuid(u.uid)?;
    }

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
        fs::set_permissions(&tmpdir, fs::Permissions::from_mode(mode.bits()))?;
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

fn bind(source: impl AsRef<Path>, target: impl AsRef<Path>, optional: bool) -> Result<()> {
  let source = source.as_ref();
  let target = target.as_ref();
  println!(
    "bind mounting '{}' to '{}'",
    source.display(),
    target.display()
  );
  let meta = match fs::metadata(source) {
    Ok(m) => m,
    Err(e) if e.kind() == std::io::ErrorKind::NotFound && optional => {
      return Ok(());
    }
    Err(e) => bail!(e),
  };
  if meta.is_dir() {
    fs::create_dir_all(target)?;
  } else {
    fs::create_dir_all(target.parent().unwrap())?;
    fs::write(target, "")?;
  }
  mount::mount(
    Some(source),
    target,
    Some(""),
    mount::MsFlags::MS_BIND | mount::MsFlags::MS_REC,
    None::<&str>,
  )?;
  Ok(())
}
