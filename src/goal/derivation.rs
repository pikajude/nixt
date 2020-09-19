use super::{Action, BuildMode, GoalLike, GoalPtr, WeakGoal, Worker};
use crate::{
  prelude::*,
  sync::{fs_lock::PathLocks, user_lock::UserLock},
};
use ipc_channel::ipc::IpcReceiver;
use settings::SandboxMode;
use std::{
  collections::{BTreeSet, HashMap, HashSet},
  io::ErrorKind::AlreadyExists,
  os::unix::{fs::PermissionsExt, io::AsRawFd, prelude::*, process::CommandExt},
  sync::atomic::{AtomicUsize, Ordering},
};
use unix::{
  fcntl::OFlag,
  pty,
  sys::{stat::Mode, statvfs::statvfs, termios, wait::WaitStatus},
  unistd,
};
#[cfg(target_os = "linux")] mod linux;
#[cfg(target_os = "macos")] mod macos;

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
enum State {
  GetDerivation,
  LoadDerivation,
  // HaveDerivation,
  OutputsSubstituted,
  // ClosureRepaired,
  InputsRealised,
  TryToBuild,
  TryLocalBuild,
  BuildDone,
}

#[derive(Debug)]
struct ChrootDir {
  path: PathBuf,
  optional: bool,
}

#[derive(Debug)]
pub struct DerivationGoal {
  state: State,
  derivation: Derivation,
  drv_path: StorePath,
  build_user: Option<UserLock>,
  closure: BTreeSet<StorePath>,
  wanted_outputs: BTreeSet<String>,
  build_mode: BuildMode,
  waitees: Vec<GoalPtr>,
  waiters: Vec<WeakGoal>,

  log_size: usize,

  chroot_root: AutoDelete,
  tmpdir: AutoDelete,
  dirs_in_chroot: HashMap<String, ChrootDir>,

  pid: Option<Pid>,
}

impl GoalLike for DerivationGoal {
  fn work(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    match self.state {
      State::GetDerivation => self.get_derivation(worker),
      State::LoadDerivation => self.load_derivation(worker),
      State::TryToBuild => self.try_to_build(worker),
      State::TryLocalBuild => self.try_local_build(worker),
      State::BuildDone => self.build_done(worker),
      x => unimplemented!("{:?}", x),
    }
  }

  fn add_waiter(&mut self, ptr: &GoalPtr) {
    self.waiters.push(Rc::downgrade(ptr))
  }

  fn key(&self) -> String {
    format!("b${}${}", self.drv_path.name, self.drv_path.to_string())
  }

  fn name(&self) -> Cow<str> {
    Cow::Borrowed(&self.drv_path.name)
  }

  fn handle_output(&mut self, _: &(), data: &[u8]) -> Result<Vec<Action>> {
    self.log_size += data.len();
    if settings().max_log_size.map_or(false, |x| self.log_size > x) {
      bail!("{} killed after writing too much output", self.key());
    }

    for line in data.split(|x| *x == b'\n').filter(|x| !x.is_empty()) {
      println!("{}: {}", self.key(), String::from_utf8_lossy(line))
    }

    Ok(vec![])
  }

  fn handle_eof(&mut self) -> Result<Vec<Action>> {
    Ok(vec![Action::Wakeup])
  }

  fn waiters(&self) -> &[WeakGoal] {
    &self.waiters
  }

  fn waitee_done(&mut self, ptr: &GoalPtr) -> Result<Vec<Action>> {
    self.waitees.retain(|x| !Rc::ptr_eq(x, ptr));
    if self.waitees.is_empty() {
      return Ok(vec![Action::Wakeup]);
    }
    Ok(vec![])
  }
}

impl DerivationGoal {
  pub fn new(path: StorePath, wanted_outputs: BTreeSet<String>, build_mode: BuildMode) -> Self {
    Self {
      state: State::GetDerivation,
      derivation: Derivation::default(),
      drv_path: path,
      chroot_root: AutoDelete(PathBuf::from("/no-such-path")),
      tmpdir: AutoDelete(PathBuf::from("/no-such-path")),
      build_user: None,
      closure: BTreeSet::default(),
      dirs_in_chroot: HashMap::new(),
      wanted_outputs,
      build_mode,
      waitees: vec![],
      waiters: vec![],
      log_size: 0,
      pid: None,
    }
  }

  pub fn add_wanted_outputs<I: IntoIterator<Item = String>>(&mut self, outputs: I) {
    self.wanted_outputs.extend(outputs)
  }

  fn get_derivation(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    debug!("init");

    if self.build_mode == BuildMode::Normal && worker.store.is_valid_path(&self.drv_path)? {
      self.load_derivation(worker)
    } else {
      let waitee = worker.make_substitution_goal(self.drv_path.clone(), false)?;
      self.waitees.push(Rc::clone(&waitee));
      self.state = State::LoadDerivation;
      Ok(vec![Action::AddToWaiters(waitee)])
    }
  }

  fn load_derivation(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    debug!("loading derivation");
    worker.store.add_temp_root(&self.drv_path)?;

    assert!(worker.store.is_valid_path(&self.drv_path)?);

    self.derivation = worker.store.read_derivation(&self.drv_path)?;

    self.have_derivation(worker)
  }

  fn have_derivation(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    debug!("have derivation");
    for path in self.derivation.outputs.values() {
      worker.store.add_temp_root(&path.path)?;
    }

    if self.waitees.is_empty() {
      self.outputs_substituted(worker)
    } else {
      self.state = State::OutputsSubstituted;
      Ok(vec![])
    }
  }

  fn outputs_substituted(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    debug!("finished attempting to substitute all outputs");

    self.wanted_outputs.clear();

    let mut needed = vec![];

    for (path, wanted_outputs) in &self.derivation.input_derivations {
      let goal =
        worker.make_derivation_goal(path.clone(), wanted_outputs.clone(), self.build_mode)?;
      self.waitees.push(Rc::clone(&goal));
      needed.push(Action::AddToWaiters(goal));
    }

    for item in &self.derivation.input_sources {
      if worker.store.is_valid_path(item)? {
        continue;
      }
      if !settings().use_substitutes {
        bail!(
          "dependency '{}' of '{}' does not exist, and substitution is disabled",
          worker.store.print_store_path(item),
          worker.store.print_store_path(&self.drv_path)
        );
      }
      let goal = worker.make_substitution_goal(item.clone(), false)?;
      self.waitees.push(Rc::clone(&goal));
      needed.push(Action::AddToWaiters(goal));
    }

    if self.waitees.is_empty() {
      self.inputs_realised(worker)
    } else {
      self.state = State::InputsRealised;
      Ok(needed)
    }
  }

  fn inputs_realised(&mut self, _: &mut Worker) -> Result<Vec<Action>> {
    self.state = State::TryToBuild;
    Ok(vec![Action::Wakeup])
  }

  fn try_to_build(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    info!("trying to build");

    let lock_files = self
      .derivation
      .out_paths()
      .map(|p| worker.store.to_real_path(p))
      .collect::<Result<Vec<_>>>()?;

    let mut locks = PathLocks::new();

    if !locks.lock(lock_files.iter(), false, None)? {
      info!("waiting for lock on {:?}", lock_files);
      return Ok(vec![Action::WaitForAwhile]);
    }

    let cur_builds = worker.local_builds;
    if cur_builds >= settings().max_build_jobs {
      locks.unlock();
      return Ok(vec![Action::WaitForBuildSlot]);
    }

    self.state = State::TryLocalBuild;
    Ok(vec![Action::Wakeup])
  }

  fn try_local_build(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    let store = &*worker.store;
    store.add_temp_root(&self.drv_path)?;

    if unistd::getuid().is_root() {
      if let Some(group) = &settings().build_users_group {
        if cfg!(unix) {
          if let Some(u) = UserLock::get_free_user(group)? {
            u.kill()?;
            self.build_user = Some(u);
          } else {
            debug!("out of UIDs, waiting");
            return Ok(vec![Action::WaitForAwhile]);
          }
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

    if self.derivation.is_builtin() {
      crate::fetch::preload_nss();
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

    if let Some(ref u) = self.build_user {
      unistd::chown(&host_tmpdir, Some(u.uid), Some(u.gid))?;
    }

    self.tmpdir = AutoDelete(host_tmpdir.clone());

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

    let actions = if use_chroot {
      self.exec_child(store, read_side.into_raw_fd(), write_side, sandbox_tmpdir)?
    } else {
      bail!("not implemented: building outside chroot")
    };

    // this is important, because otherwise the worker loop will not get EOF
    unistd::close(write_side)?;
    self.state = State::BuildDone;
    Ok(actions)
  }

  fn build_done(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    let mut acts = vec![Action::ChildTerminated {
      wake_sleepers: false,
    }];
    trace!("build done");

    let status = self
      .pid
      .take()
      .expect("pid not set in build_done()")
      .kill()?;

    debug!(
      "builder process for `{}' finished: {:?}",
      worker.store.print_store_path(&self.drv_path),
      status
    );

    warn!("TODO: close the log file");

    if let Some(ref u) = self.build_user {
      u.kill()?;
    }

    warn!("TODO: stop the recursive daemon");

    let mut disk_full = false;

    if !matches!(status, WaitStatus::Exited(_, 0)) {
      const REQUIRED_SPACE: u64 = 8 * 1024 * 1024;
      if let Ok(s) = statvfs(&*worker.store.store_path()) {
        if s.blocks_available() * s.block_size() < REQUIRED_SPACE {
          disk_full = true;
        }
      }
      if let Ok(s) = statvfs(&self.tmpdir) {
        if s.blocks_available() * s.block_size() < REQUIRED_SPACE {
          disk_full = true;
        }
      }

      let mut msg = format!(
        "builder for '{}' {}",
        worker.store.print_store_path(&self.drv_path),
        crate::util::show_status(status)
      );

      if disk_full {
        msg.push_str("\nnote: failure may have been caused by lack of free disk space");
      }

      acts.push(Action::Done(Some(anyhow!(msg))));
    }

    self.register_outputs(worker)?;

    // this closes the fd that the userlock holds
    self.build_user.take();

    Ok(acts)
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

  fn register_outputs(&self, worker: &mut Worker) -> Result<()> {
    for out in self.derivation.outputs.values() {
      worker.store.register_valid_path(ValidPathInfo::new(
        out.path.clone(),
        Hash::hash_str("foo", HashType::SHA256),
      ))?;
    }
    Ok(())
  }
}

struct DerivationWorker<'a> {
  use_chroot: bool,
  private_network: bool,
  user_ns_sync: IpcReceiver<()>,
  chroot_root: PathBuf,
  sandbox_tmpdir: PathBuf,
  dirs_in_chroot: &'a HashMap<String, ChrootDir>,
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

    /*
    if let Some(u) = self.build_user {
      // no nix::setgroups on macos because fuck you
      if !u.other_gids.is_empty()
        && unsafe { libc::setgroups(u.other_gids.len() as _, u.other_gids.as_ptr().cast()) == -1 }
      {
        bail!(std::io::Error::last_os_error());
      }
      unistd::setgid(u.gid)?;
      unistd::setuid(u.uid)?;
    }
    */

    unistd::chdir(sandbox_tmpdir)
      .with_context(|| format!("trying to move into tmpdir `{}'", sandbox_tmpdir.display()))?;

    // see comment below
    eprintln!("\x01");

    if derivation.is_builtin() {
      let maybe_failure: Result<()> = try {
        if derivation.builder.to_str() == Some("builtin:fetchurl") {
          crate::fetch::fetchurl(&derivation)?;
        } else {
          bail!("unknown builtin: {}", derivation.builder.display());
        }
        std::process::exit(0);
      };
      if let Err(e) = maybe_failure {
        eprintln!("error: {}", e);
        std::process::exit(1);
      }
    }

    let mut cmd = std::process::Command::new(derivation.builder.as_os_str());

    cmd.arg0(
      derivation
        .builder
        .file_name()
        .expect("builder should always have a filename"),
    );
    cmd.args(derivation.args.iter());

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
