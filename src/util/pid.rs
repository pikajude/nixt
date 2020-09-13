use crate::prelude::*;
use unistd::Pid as NixPid;
use unix::{
  errno::Errno,
  sys::{
    signal::{kill, killpg, Signal},
    wait::{waitpid, WaitStatus},
  },
  unistd,
};

#[derive(Debug)]
pub struct Pid {
  pub pid: NixPid,
  separate_process_group: bool,
  kill_signal: Signal,
}

impl Pid {
  pub fn new(pid: libc::pid_t) -> Self {
    Self {
      pid: NixPid::from_raw(pid),
      separate_process_group: false,
      kill_signal: Signal::SIGKILL,
    }
  }

  pub fn set_separate_pg(&mut self, pg: bool) {
    self.separate_process_group = pg;
  }

  pub fn set_kill_signal(&mut self, signal: Signal) {
    self.kill_signal = signal;
  }

  pub fn kill(self) -> Result<WaitStatus> {
    debug!("killing process {}", self.pid);

    let kill_fn = if self.separate_process_group {
      killpg
    } else {
      kill
    };

    kill_fn(self.pid, Some(self.kill_signal)).or_else(|e| {
      if cfg!(any(target_os = "macos", target_os = "freebsd")) && e.as_errno() == Some(Errno::EPERM)
      {
        kill(self.pid, None)
      } else {
        Err(e)
      }
    })?;

    self.wait()
  }

  pub fn wait(self) -> Result<WaitStatus> {
    Ok(waitpid(self.pid, None)?)
  }
}

pub fn show_status(w: WaitStatus) -> String {
  match w {
    WaitStatus::Exited(_, 0) => "succeeded".into(),
    WaitStatus::Exited(_, n) => format!("failed with exit code {}", n),
    WaitStatus::Signaled(_, sig, _coredump) => format!("failed due to signal {}", sig),
    _ => "died abnormally".into(),
  }
}
