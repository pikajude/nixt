use crate::{prelude::*, sync::user_lock::UserLock};
use derivation::DerivationGoal;
use settings::SandboxMode;
use std::{
  collections::{HashMap, HashSet},
  io::ErrorKind::AlreadyExists,
  os::unix::{fs::PermissionsExt, io::AsRawFd, prelude::RawFd, process::CommandExt},
  sync::atomic::{AtomicUsize, Ordering},
  time::Instant,
};
use unix::{
  fcntl::OFlag,
  pty,
  sys::{stat::Mode, termios},
  unistd,
};

pub mod derivation;

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum ExitCode {
  Busy,
  Success,
  Failure,
  NoSubstituters,
  IncompleteClosure,
}

pub struct Worker<'a> {
  pub store: &'a dyn Store,
  pub goals: Vec<Goal>,
}

impl<'a> Worker<'a> {
  pub fn new(store: &'a dyn Store) -> Self {
    Self {
      store,
      goals: vec![],
    }
  }

  pub fn run(self) -> Result<()> {
    let completion = crossbeam::scope(|s| {
      let mut fds = vec![];
      for goal in self.goals {
        let s_ref = self.store;
        match goal {
          Goal::Derivation(mut d) => {
            let handle = s.spawn(move |_| {
              let child = d.local_build(s_ref)?;
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
      fds.into_iter().try_for_each(|x| {
        // scope() returns Result<..., Panic>, but handle.join() also returns
        // Result<..., Panic> although having both is redundant
        x.join().unwrap_or_else(|e| panic!(e))
      })
    });
    match completion {
      Ok(x) => x,
      Err(e) => {
        if let Some(x) = e.downcast_ref::<String>() {
          bail!("{}", x)
        } else {
          bail!("unspecified error from child")
        }
      }
    }
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
