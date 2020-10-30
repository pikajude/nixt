use std::{os::unix::fs::symlink, ptr};

use ipc_channel::ipc::{self, IpcReceiver};
use unix::{
  mount::*,
  sched::{clone, unshare, CloneFlags},
  sys::{
    mman::{mmap, MapFlags, ProtFlags},
    stat::Mode,
    wait::*,
  },
  unistd,
};

use crate::{
  store::ClosureOpts,
  sync::{fs_lock::PathLocks, user_lock::UserLock},
};

use super::*;

const SANDBOX_UID: u32 = 1000;
const SANDBOX_GID: u32 = 100;

pub(super) fn exec_builder(
  store: &dyn Store,
  messages: &Arc<Queue<Message>>,
  scope: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
  progress: &Arc<MultiProgress>,
) -> Result<Option<FinishedChild>> {
  let build_log_path = store.logfile_of(path);
  std::fs::create_dir_all(build_log_path.parent().unwrap())?;

  let mut input_paths = BTreeSet::new();
  for (input_path, outputs) in &drv.input_derivations {
    let input_drv = store.read_derivation(input_path)?;
    for out in outputs {
      if let Some(out) = input_drv.outputs.get(out) {
        store.compute_closure(&out.path, &mut input_paths, ClosureOpts::default())?;
      } else {
        bail!(
          "derivation {} requires nonexistent output {} from derivation {}",
          path,
          out,
          input_path
        );
      }
    }
  }

  for src in &drv.input_sources {
    store.compute_closure(src, &mut input_paths, ClosureOpts::default())?;
  }

  debug!("added input paths: {:?}", input_paths);

  let lock_files = drv
    .out_paths()
    .map(|s| store.to_real_path(s))
    .collect::<Result<Vec<_>>>()?;
  let _path_locks = PathLocks::new().lock(&lock_files, true, None)?;

  let build_user = match settings().build_users_group {
    Some(ref u) => UserLock::get_free_user(u)?,
    None => None,
  };

  let builder_tmp = tempfile::Builder::new()
    .prefix(format!("nix-build-{}-", drv.name).as_str())
    .tempdir()?;

  let mut dirs_in_chroot = settings()
    .sandbox_paths
    .union(&settings().extra_sandbox_paths)
    .map(|dir| {
      let mut dir = dir.as_str();
      let mut optional = false;
      if let Some(dir2) = dir.strip_suffix('?') {
        dir = dir2;
        optional = true;
      }
      match break_str(dir, '=') {
        Some((lhs, rhs)) => (
          Cow::Borrowed(Path::new(lhs)),
          (Cow::Borrowed(Path::new(rhs)), optional),
        ),
        None => (
          Cow::Borrowed(Path::new(dir)),
          (Cow::Borrowed(Path::new(dir)), optional),
        ),
      }
    })
    .collect::<HashMap<_, _>>();

  dirs_in_chroot.insert(
    Cow::Borrowed(settings().sandbox_build_dir.as_ref()),
    (Cow::Borrowed(builder_tmp.as_ref()), false),
  );

  let mut extra_dirs_closure = BTreeSet::new();
  for (path, _) in dirs_in_chroot.values() {
    if let Ok(x) = store.parse_store_path(path) {
      if store.is_valid_path(&x).unwrap_or(false) {
        store.compute_closure(&x, &mut extra_dirs_closure, Default::default())?;
      }
    }
  }

  for obj in extra_dirs_closure {
    let dest = store.to_real_path(&obj)?;
    dirs_in_chroot.insert(Cow::Owned(dest.clone()), (Cow::Owned(dest), false));
  }

  let chroot_root_dir = store.to_real_path(path)?.with_extension("drv.chroot");
  rm_rf(&chroot_root_dir)?;
  debug!(
    "setting up chroot environment in {}",
    chroot_root_dir.display()
  );
  fs::create_dir_all(&chroot_root_dir)?;
  fs::create_dir_all(chroot_root_dir.join("tmp"))?;
  fs::create_dir_all(chroot_root_dir.join("etc"))?;
  fs::write(
    chroot_root_dir.join("etc/passwd"),
    format!(
      "root:x:0:0:Nix build user:{2}:/noshell\nnixbld:x:{0}:{1}:Nix build \
       user:{2}:/noshell\nnobody:x:65534:65534:Nobody:/:/noshell\n",
      SANDBOX_UID,
      SANDBOX_GID,
      settings().sandbox_build_dir.display()
    ),
  )?;
  fs::write(
    chroot_root_dir.join("etc/group"),
    format!("root:x:0:\nnixbld:!:{}:\nnogroup:x:65534:\n", SANDBOX_GID),
  )?;
  if !drv.is_builtin() {
    fs::write(
      chroot_root_dir.join("etc/hosts"),
      "127.0.0.1 localhost\n::1 localhost\n",
    )?;
  }
  let chroot_store =
    chroot_root_dir.join(Path::new(&*store.store_path()).strip_prefix("/").unwrap());
  fs::create_dir_all(&chroot_store)?;

  for p in &input_paths {
    let real_path = store.to_real_path(p)?;
    if !real_path.exists() {
      bail!(
        "unable to check validity of input path {}",
        real_path.display()
      );
    }
    if real_path.is_dir() {
      dirs_in_chroot.insert(
        Cow::Owned(real_path.clone()),
        (Cow::Owned(real_path), false),
      );
    } else {
      fs::hard_link(
        &real_path,
        chroot_root_dir.join(real_path.strip_prefix("/").unwrap()),
      )?;
    }
  }

  for out in drv.outputs.values() {
    if let Ok(x) = store.to_real_path(&out.path) {
      dirs_in_chroot.remove(x.as_path());
    }
  }

  // stdout pipes for the builder process
  let (pipe_read, pipe_write) = unistd::pipe2(OFlag::O_CLOEXEC)?;
  // pipe used for the builder to send messages directly to our logger (rather
  // than its log file)
  let (log_read, log_write) = unistd::pipe()?;
  scope.spawn(move |_| {
    let f = BufReader::new(unsafe { fs::File::from_raw_fd(log_read) });
    for l in f.lines() {
      info!("{}", l?);
    }
    Ok::<_, io::Error>(())
  });

  let mut cmd = mk_command(store, drv)?;
  cmd.stdin(Stdio::null());
  unsafe {
    cmd.stdout(Stdio::from_raw_fd(pipe_write));
    cmd.stderr(Stdio::from_raw_fd(pipe_write));
  }

  let (pid_send, pid_receive) = ipc::channel::<i32>()?;
  let (user_ns_send, user_ns_receive) = ipc::channel::<()>()?;

  if unistd::fork()
    .context("can't create a child process")?
    .is_child()
  {
    // logic copied from nix upstream. can we use a slice instead? I don't know how
    // the mmap flags correspond
    let stack_size = 1024 * 1024;
    let stack_ptr = unsafe {
      mmap(
        ptr::null_mut(),
        stack_size,
        ProtFlags::PROT_READ | ProtFlags::PROT_WRITE,
        MapFlags::MAP_PRIVATE | MapFlags::MAP_ANONYMOUS | MapFlags::MAP_STACK,
        -1,
        0,
      )
      .with_context(|| "unable to allocate stack for child process")?
    };
    let stack = unsafe { std::slice::from_raw_parts_mut(stack_ptr as *mut u8, stack_size) };
    let mut clone_flags = CloneFlags::CLONE_NEWUSER
      | CloneFlags::CLONE_NEWPID
      | CloneFlags::CLONE_NEWNS
      | CloneFlags::CLONE_NEWIPC
      | CloneFlags::CLONE_NEWUTS
      | CloneFlags::CLONE_PARENT;
    if !drv.is_fixed_output() {
      clone_flags |= CloneFlags::CLONE_NEWNET;
    }

    let pid = clone(
      Box::new(|| {
        try_run_child(
          store,
          drv,
          &mut cmd,
          &user_ns_receive,
          &chroot_root_dir,
          log_write,
          &mut dirs_in_chroot,
        )
      }),
      stack,
      clone_flags,
      Some(libc::SIGCHLD),
    )?;
    pid_send.send(pid.as_raw())?;
    std::process::exit(0);
  }
  unistd::close(log_write)?;

  let pid = pid_receive.recv().map_err(|x| anyhow!("{:?}", x))?;

  let progress = progress.insert(
    0,
    ProgressBar::new_spinner().with_style(
      ProgressStyle::default_spinner().template("[{elapsed_precise}] {prefix:.green} {wide_msg}"),
    ),
  );
  progress.set_prefix(&drv.name);
  progress.enable_steady_tick(1000);

  let p2 = progress.clone();
  scope.spawn(move |_| Logger::new(build_log_path, pipe_read, p2)?.run());

  messages.push(Message::SpawnedProcess(pid as _));

  let (host_uid, host_gid) = build_user
    .as_ref()
    .map_or_else(|| (unistd::getuid(), unistd::getgid()), |u| (u.uid, u.gid));

  let procfs = Path::new("/proc").join(pid.to_string());
  fs::write(
    procfs.join("uid_map"),
    format!("{} {} 1", SANDBOX_UID, host_uid),
  )?;
  fs::write(procfs.join("setgroups"), "deny")?;
  fs::write(
    procfs.join("gid_map"),
    format!("{} {} 1", SANDBOX_GID, host_gid),
  )?;
  let _ns_fd = fs::File::open(procfs.join("ns").join("mnt"))?;

  // signal the builder that it can go ahead
  user_ns_send.send(())?;

  match waitpid(Some(unistd::Pid::from_raw(pid)), None)? {
    WaitStatus::Exited(_, s) => {
      if s > 0 {
        bail!(
          "builder for {} failed with status {}",
          store.print_store_path(path),
          s
        );
      }
    }
    s => bail!("unexpected wait status from child: {:?}", s),
  }

  progress.set_message("registering outputs");

  // Register the build outputs.
  let mut referenceable_paths = HashSet::<StorePath>::new();

  for out in drv.outputs.values() {
    referenceable_paths.insert(out.path.clone());

    let real_path = store.print_store_path(&out.path);
    let chroot_path = chroot_root_dir.join(Path::new(&real_path).strip_prefix("/").unwrap());
    if chroot_path.exists() {
      fs::rename(chroot_path, real_path)?;
    }
  }

  for output in drv.outputs.values() {
    let out_path = store.print_store_path(&output.path);

    canonicalise_path_metadata(&out_path, None)?;

    let mut path_hash = crate::hash::Sink::new(HashType::SHA256);
    let mut scanner = crate::archive::RefsScanner::new(referenceable_paths.iter().map(|p| p.hash));

    debug!("dumping {}", out_path);

    crate::archive::dump_path(
      &out_path,
      TeeWriter::new(&mut path_hash, &mut scanner),
      &PathFilter::none(),
    )?;

    let (path_hash, _) = path_hash.finish();

    debug!("output has hash {}", path_hash.encode(Encoding::Base32));

    store.register_valid_path(ValidPathInfo::new(output.path.clone(), path_hash))?;
  }

  progress.finish_and_clear();
  Ok(Some(FinishedChild(pid as _)))
}

fn mk_command(store: &dyn Store, drv: &Derivation) -> Result<Command> {
  let mut rewrites = HashMap::new();

  for (name, output) in &drv.outputs {
    let out_path = store.print_store_path(&output.path);

    rewrites.insert(crate::derivation::hash_placeholder(name), out_path);
  }

  let mut cmd = Command::new(drv.builder.as_os_str());
  cmd.arg0(&drv.args[0]);
  cmd.args(&drv.args[1..]);
  cmd.env_clear();

  for (ekey, eval) in &drv.env {
    let mut eval_ = eval.to_owned();
    for (find, replace) in &rewrites {
      eval_ = eval_.replace(find, replace);
    }
    cmd.env(ekey, eval_);
  }

  cmd.env("PATH", "/path-not-set");
  cmd.env("HOME", "/homeless-shelter");
  cmd.env("NIX_STORE", store.store_path());
  cmd.env("NIX_BUILD_CORES", settings().build_cores.to_string());
  cmd.env("NIX_LOG_FD", "2");
  cmd.env("TERM", "xterm-256color");

  for alias in &["NIX_BUILD_TOP", "TMPDIR", "TEMPDIR", "TMP", "TEMP", "PWD"] {
    cmd.env(alias, "/build");
  }

  Ok(cmd)
}

const NULLSTR: Option<&'static str> = None;

fn try_run_child(
  store: &dyn Store,
  drv: &Derivation,
  command: &mut Command,
  user_ns_ok: &IpcReceiver<()>,
  chroot_root_dir: &Path,
  logger_fd: RawFd,
  dirs_in_chroot: &mut HashMap<Cow<Path>, (Cow<Path>, bool)>,
) -> isize {
  // redirects print! and eprint! to the logger
  unistd::dup2(logger_fd, io::stderr().as_raw_fd()).unwrap();
  unistd::dup2(logger_fd, io::stdout().as_raw_fd()).unwrap();

  let mut result = move || {
    common_child_init()?;

    // wait for (mount) namespace initialization
    user_ns_ok
      .recv()
      .map_err(|_| anyhow!("unable to initialize user namespace"))?;

    unistd::sethostname("localhost")?;
    unsafe {
      if libc::setdomainname(b"(none)".as_ptr() as *const _, b"(none)".len()) != 0 {
        bail!("unable to set domain name to (none)");
      }
    }

    mount(
      NULLSTR,
      "/",
      NULLSTR,
      MsFlags::MS_PRIVATE | MsFlags::MS_REC,
      NULLSTR,
    )
    .with_context(|| "unable to make `/` private")?;

    mount(
      Some(chroot_root_dir),
      chroot_root_dir,
      NULLSTR,
      MsFlags::MS_BIND,
      NULLSTR,
    )
    .with_context(|| "unable to bind mount the chroot directory")?;

    let chroot_store_dir =
      chroot_root_dir.join(Path::new(&*store.store_path()).strip_prefix("/").unwrap());

    mount(
      Some(&chroot_store_dir),
      &chroot_store_dir,
      NULLSTR,
      MsFlags::MS_BIND,
      NULLSTR,
    )
    .with_context(|| "unable to bind mount the Nix store")?;
    mount(
      NULLSTR,
      &chroot_store_dir,
      NULLSTR,
      MsFlags::MS_SHARED,
      NULLSTR,
    )
    .with_context(|| "unable to make the store dir shared")?;

    let mut ss = vec![];
    if !dirs_in_chroot.contains_key(Path::new("/dev")) {
      fs::create_dir_all(chroot_root_dir.join("dev/shm"))?;
      fs::create_dir_all(chroot_root_dir.join("dev/pts"))?;
      if settings().system_features.contains("kvm") && Path::new("/dev/kvm").exists() {
        ss.push("/dev/kvm");
      }
      ss.extend(
        [
          "/dev/full",
          "/dev/null",
          "/dev/random",
          "/dev/tty",
          "/dev/urandom",
          "/dev/zero",
        ]
        .iter(),
      );
      symlink("/proc/self/fd", chroot_root_dir.join("dev/fd"))?;
      symlink("/proc/self/fd/0", chroot_root_dir.join("dev/stdin"))?;
      symlink("/proc/self/fd/1", chroot_root_dir.join("dev/stdout"))?;
      symlink("/proc/self/fd/2", chroot_root_dir.join("dev/stderr"))?;
    }

    if drv.is_builtin() {
      ss.push("/etc/resolv.conf");
      fs::write(
        chroot_root_dir.join("etc/nsswitch.conf"),
        "hosts: files dns\nservices: files\n",
      )?;
      ss.push("/etc/services");
      ss.push("/etc/hosts");
      if Path::new("/var/run/nscd/socket").exists() {
        ss.push("/var/run/nscd/socket");
      }
    }

    for s in ss {
      dirs_in_chroot.insert(
        Cow::Borrowed(Path::new(s)),
        (Cow::Borrowed(Path::new(s)), false),
      );
    }

    let do_bind = |source: &Path, target: &Path, optional: bool| -> Result<()> {
      println!(
        "bind mounting `{}` to `{}`",
        source.display(),
        target.display()
      );
      if optional && !source.exists() {
        return Ok(());
      }
      if source.is_dir() {
        fs::create_dir_all(target)?;
      } else {
        fs::create_dir_all(target.parent().unwrap())?;
        fs::write(target, "")?;
      }
      mount(
        Some(source),
        target,
        Some(""),
        MsFlags::MS_BIND | MsFlags::MS_REC,
        NULLSTR,
      )
      .context("while trying to bind mount")?;
      Ok(())
    };

    for (to, (from, optional)) in dirs_in_chroot.iter() {
      if from == Path::new("/proc") {
        continue;
      }
      do_bind(
        from,
        chroot_root_dir
          .join(to.strip_prefix("/").unwrap_or(to))
          .as_path(),
        *optional,
      )?;
    }

    fs::create_dir_all(chroot_root_dir.join("proc"))?;
    mount(
      Some("none"),
      chroot_root_dir.join("proc").as_path(),
      Some("proc"),
      MsFlags::empty(),
      NULLSTR,
    )
    .context("mounting procfs")?;

    if Path::new("/dev/shm").exists() {
      mount(
        Some("none"),
        chroot_root_dir.join("dev/shm").as_path(),
        Some("tmpfs"),
        MsFlags::empty(),
        Some(format!("size={}", settings().sandbox_shm_size).as_str()),
      )
      .context("mounting /dev/shm")?;
    }

    if Path::new("/dev/pts/ptmx").exists()
      && !chroot_root_dir.join("dev/ptmx").exists()
      && !dirs_in_chroot.contains_key(Path::new("/dev/pts"))
    {
      mount(
        Some("none"),
        chroot_root_dir.join("dev/pts").as_path(),
        Some("devpts"),
        MsFlags::empty(),
        Some("newinstance,mode=0620"),
      )
      .context("mounting /dev/pts")?;
    }

    unshare(CloneFlags::CLONE_NEWNS)?;
    unistd::chdir(chroot_root_dir)?;
    unistd::mkdir("real-root", Mode::empty())?;
    unistd::pivot_root(".", "real-root")?;
    unistd::chroot(".")?;
    umount2("real-root", MntFlags::MNT_DETACH)?;
    std::fs::remove_dir("real-root")?;

    unistd::setgid(unistd::Gid::from_raw(SANDBOX_GID))?;
    unistd::setuid(unistd::Uid::from_raw(SANDBOX_UID))?;

    command.env("PWD", "/build");
    command.current_dir("/build");
    let stat = command.status().with_context(|| {
      format!(
        "while executing build command `{}`",
        command.get_program().to_string_lossy()
      )
    })?;
    if !stat.success() {
      bail!("builder failed with status {}", stat.code().unwrap_or(-1));
    }
    Ok(())
  };

  let r = result();
  unistd::close(logger_fd).unwrap();

  if let Err(e) = r {
    eprintln!("{:#}", e);
    1
  } else {
    0
  }
}

fn common_child_init() -> Result<()> {
  unistd::setsid().with_context(|| "unable to create a new session")?;

  // nix upstream uses dup2 to send stdout and stderr to the RawFd, but Command
  // already does that for us, so this method doesn't need to do anything else

  Ok(())
}
