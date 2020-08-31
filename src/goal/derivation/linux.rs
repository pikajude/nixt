use super::{DerivationGoal, DerivationWorker};
use crate::{
  goal::{Action, Child},
  prelude::*,
};
use linux_personality::{get_personality as get, personality as put, Personality};
use std::{
  io::{BufRead, Error, Write},
  os::unix::{
    fs::{symlink, PermissionsExt},
    io::*,
  },
};
use unix::{
  mount::{self, MntFlags, MsFlags},
  pty::PtyMaster,
  sched::{self, CloneFlags},
  sys::{
    mman::{self, MapFlags, ProtFlags},
    socket, wait,
  },
  unistd,
};

pub const SANDBOX_GID: u32 = 100;
pub const SANDBOX_UID: u32 = 1000;

fn personality(p: Personality) -> Result<Personality> {
  Ok(put(p).map_err(|_| Error::last_os_error())?)
}

fn get_personality() -> Result<Personality> {
  Ok(get().map_err(|_| Error::last_os_error())?)
}

impl DerivationGoal {
  pub fn setup_chroot(&mut self, store: &dyn Store) -> Result<()> {
    self.chroot_root = store.to_real_path(&self.drv_path)?.with_extension("chroot");

    debug!(
      "setting up chroot environment in `{}'",
      self.chroot_root.display()
    );

    if let Err(e) = fs::remove_dir_all(&self.chroot_root) {
      if e.kind() != io::ErrorKind::NotFound {
        bail!(e)
      }
    }
    fs::create_dir_all(&self.chroot_root)?;
    fs::set_permissions(&self.chroot_root, fs::Permissions::from_mode(0o750))?;

    if let Some(u) = &self.build_user {
      unistd::chown(&self.chroot_root, None, Some(u.gid)).with_context(|| {
        anyhow!(
          "while trying to chown dir `{}' to build-user `{}'",
          self.chroot_root.display(),
          u.uid
        )
      })?;
    }

    let chroot_tmp = self.chroot_root.join("tmp");
    fs::create_dir_all(&chroot_tmp)?;
    fs::set_permissions(&chroot_tmp, fs::Permissions::from_mode(0o1777))?;

    fs::create_dir_all(self.chroot_root.join("etc"))?;
    fs::write(
      self.chroot_root.join("etc").join("passwd"),
      format!(
        "root:x:0:0:Nix build user:{2}:/noshell\nnixbld:x:{0}:{1}:Nix build \
         user:{2}:/noshell\nnobody:x:65534:65534:Nobody:/:/noshell\n",
        SANDBOX_UID,
        SANDBOX_GID,
        settings().sandbox_build_dir.display()
      ),
    )?;

    fs::write(
      self.chroot_root.join("etc").join("group"),
      format!("root:x:0:\nnixbld:!:{}:\nnogroup:x:65534:\n", SANDBOX_GID),
    )?;

    fs::write(
      self.chroot_root.join("etc").join("hosts"),
      "127.0.0.1 localhost\n::1 localhost\n",
    )?;

    let chroot_store_dir = self.chroot_root.join(
      AsRef::<Path>::as_ref(&store.store_path())
        .strip_prefix("/")
        .expect("nix store path should be absolute"),
    );
    fs::create_dir_all(&chroot_store_dir)?;
    fs::set_permissions(&chroot_store_dir, fs::Permissions::from_mode(0o1775))?;

    if let Some(u) = &self.build_user {
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
        store.compute_closure(&path.path, &mut self.closure, false, false, false)?;
      }
    }

    for path in &self.derivation.input_sources {
      store.compute_closure(path, &mut self.closure, false, false, false)?;
    }

    debug!("derivation requires input paths: {:?}", &self.closure);

    for output in self.derivation.outputs.values() {
      self
        .dirs_in_chroot
        .remove(&store.print_store_path(&output.path));
    }

    Ok(())
  }

  pub fn exec_child(
    &mut self,
    store: &dyn Store,
    read_side: PtyMaster,
    write_side: i32,
    sandbox_tmpdir: &PathBuf,
  ) -> Result<Vec<Action>> {
    match unistd::fork()? {
      unistd::ForkResult::Parent { child } => match wait::waitpid(child, None)? {
        wait::WaitStatus::Exited(_, 0) => {
          let mut reader =
            std::io::BufReader::new(unsafe { fs::File::from_raw_fd(read_side.as_raw_fd()) })
              .lines();

          let mut child_pid = if let Some(line) = reader.next() {
            Pid::new(line?.parse()?)
          } else {
            bail!("no output from child")
          };

          child_pid.set_separate_pg(true);

          let host_uid = self.build_user.as_ref().map_or(unistd::getuid(), |x| x.uid);
          let host_gid = self.build_user.as_ref().map_or(unistd::getgid(), |x| x.gid);

          fs::write(
            format!("/proc/{}/uid_map", child_pid.pid),
            format!("{} {} 1", SANDBOX_UID, host_uid),
          )?;

          fs::write(format!("/proc/{}/setgroups", child_pid.pid), "deny")?;

          fs::write(
            format!("/proc/{}/gid_map", child_pid.pid),
            format!("{} {} 1", SANDBOX_GID, host_gid),
          )?;

          fs::OpenOptions::new()
            .read(true)
            .write(false)
            .open(format!("/proc/{}/ns/mnt", child_pid.pid))?;

          self.pid = Some(child_pid);

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
              None => debug!("{}", builder_line),
            }
          }

          // we have to return the fd to the parent, but File will close it on drop
          std::mem::forget(reader);

          Ok(vec![Action::RegisterChild(Child {
            respect_timeouts: true,
            in_build_slot: true,
            fds: vec![read_side.into_raw_fd()],
          })])
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
            ProtFlags::PROT_WRITE | ProtFlags::PROT_READ,
            MapFlags::MAP_PRIVATE | MapFlags::MAP_ANONYMOUS | MapFlags::MAP_STACK,
            -1,
            0,
          )?
          .cast::<u8>()
        };
        let stack = unsafe { std::slice::from_raw_parts_mut(stack, stack_size) };
        let child = sched::clone(
          Box::new(move || {
            match (DerivationWorker {
              use_chroot: true,
              chroot_root: self.chroot_root.clone(),
              sandbox_tmpdir: sandbox_tmpdir.to_path_buf(),
              dirs_in_chroot: &self.dirs_in_chroot,
              build_user: self.build_user.as_ref(),
              write_side,
            }
            .run(store, &self.derivation))
            {
              Ok(()) => 0,
              Err(e) => {
                eprintln!("\x01while setting up the build environment: {}", e);
                1
              }
            }
          }),
          stack,
          CloneFlags::CLONE_NEWUSER
            | CloneFlags::CLONE_NEWPID
            | CloneFlags::CLONE_NEWNS
            | CloneFlags::CLONE_NEWIPC
            | CloneFlags::CLONE_NEWUTS
            | CloneFlags::CLONE_PARENT,
          Some(libc::SIGCHLD),
        )?;

        unsafe { fs::File::from_raw_fd(write_side) }
          .write_all(format!("{}\n", child).as_bytes())?;

        std::process::exit(0)
      }
    }
  }
}

impl super::WorkerImpl for DerivationWorker<'_> {
  fn init_chroot(&self, store: &dyn Store, derivation: &Derivation) -> Result<()> {
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

    mount::mount(null, "/", null, MsFlags::MS_PRIVATE | MsFlags::MS_REC, null)?;

    mount::mount(
      Some(self.chroot_root.as_path()),
      self.chroot_root.as_path(),
      null,
      MsFlags::MS_BIND,
      null,
    )?;

    let chroot_store = self.chroot_root.join(
      AsRef::<Path>::as_ref(&store.store_path())
        .strip_prefix("/")
        .expect("nix store path should be absolute"),
    );
    mount::mount(
      Some(chroot_store.as_path()),
      chroot_store.as_path(),
      null,
      MsFlags::MS_BIND,
      null,
    )?;

    mount::mount(null, chroot_store.as_path(), null, MsFlags::MS_SHARED, null)?;

    let mut extra_dirs = vec![];
    if derivation.is_fixed_output() {
      extra_dirs.push("/etc/resolv.conf");
      fs::write(
        self.chroot_root.join("etc").join("nsswitch.conf"),
        "hosts: files dns\nservices: files\n",
      )?;
      extra_dirs.push("/etc/services");
      extra_dirs.push("/etc/hosts");
      if fs::metadata("/var/run/nscd/socket").is_ok() {
        extra_dirs.push("/var/run/nscd/socket");
      }
    }

    for (target, dir) in self.dirs_in_chroot {
      let source = dir.path.as_path();
      if source.to_str() == Some("/proc") {
        continue;
      }
      bind(
        source,
        self
          .chroot_root
          .join(Path::new(&target).strip_prefix("/").unwrap()),
        dir.optional,
      )?;
    }

    for path in extra_dirs {
      bind(
        path,
        self
          .chroot_root
          .join(Path::new(path).strip_prefix("/").unwrap()),
        false,
      )?;
    }

    fs::create_dir_all(self.chroot_root.join("dev").join("shm"))?;
    fs::create_dir_all(self.chroot_root.join("dev").join("pts"))?;
    if settings().system_features.contains("kvm") {
      fs::create_dir_all(self.chroot_root.join("dev").join("kvm"))?;
    }
    symlink("/proc/self/fd", self.chroot_root.join("dev").join("fd"))?;
    symlink(
      "/proc/self/fd/0",
      self.chroot_root.join("dev").join("stdin"),
    )?;
    symlink(
      "/proc/self/fd/1",
      self.chroot_root.join("dev").join("stdout"),
    )?;
    symlink(
      "/proc/self/fd/2",
      self.chroot_root.join("dev").join("stderr"),
    )?;

    fs::create_dir_all(self.chroot_root.join("proc"))?;
    mount::mount(
      Some("none"),
      &self.chroot_root.join("proc"),
      Some("proc"),
      MsFlags::empty(),
      null,
    )?;

    if fs::metadata("/dev/shm").is_ok() {
      mount::mount(
        Some("none"),
        &self.chroot_root.join("dev").join("shm"),
        Some("tmpfs"),
        MsFlags::empty(),
        Some(format!("size={}", settings().sandbox_shm_size).as_str()),
      )?;
    }

    if fs::metadata("/dev/pts/ptmx").is_ok() {
      mount::mount(
        Some("none"),
        &self.chroot_root.join("dev").join("pts"),
        Some("devpts"),
        MsFlags::empty(),
        Some("newinstance,mode=0620"),
      )?;
      symlink("/dev/pts/ptmx", self.chroot_root.join("dev").join("ptmx"))?;
      fs::set_permissions(
        self.chroot_root.join("dev").join("pts").join("ptmx"),
        fs::Permissions::from_mode(0o666),
      )?;
    }

    sched::unshare(CloneFlags::CLONE_NEWNS)?;
    unistd::chdir(self.chroot_root.as_path())?;
    fs::create_dir("real-root")?;
    unistd::pivot_root(".", "real-root")?;
    unistd::chroot(".")?;
    mount::umount2("real-root", MntFlags::MNT_DETACH)?;
    fs::remove_dir("real-root")?;
    unistd::setgid(unistd::Gid::from_raw(SANDBOX_GID))?;
    unistd::setuid(unistd::Uid::from_raw(SANDBOX_UID))?;

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

    Ok(())
  }
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
    MsFlags::MS_BIND | MsFlags::MS_REC,
    None::<&str>,
  )?;
  Ok(())
}
