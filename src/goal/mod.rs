use crate::{prelude::*, sync::user_lock::UserLock};
use derivation::DerivationGoal;
use linux_personality::Personality;
use settings::SandboxMode;
use std::{
  collections::{HashMap, HashSet},
  io::{BufRead, ErrorKind::AlreadyExists, Write},
  os::unix::{
    fs::{symlink, PermissionsExt},
    io::{AsRawFd, FromRawFd},
    prelude::RawFd,
    process::CommandExt,
  },
  sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
  },
  time::Instant,
};
use unix::{
  fcntl::OFlag,
  mount, pty, sched,
  sys::{mman, socket, stat::Mode, termios, wait},
  unistd,
};

pub mod derivation;

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
