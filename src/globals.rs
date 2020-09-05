use crate::prelude::*;
use libc::TIOCGWINSZ;
use std::{mem::MaybeUninit, sync::Mutex};
use unix::{
  pty::Winsize,
  sys::{
    signal::*,
    stat::{umask, Mode},
  },
};

unix::ioctl_read_bad!(winsize_syscall, TIOCGWINSZ, Winsize);

lazy_static! {
  static ref WINSIZE: Mutex<(u16, u16)> = Mutex::new(get_win_size());
  static ref SAVED_SIGNAL_MASK: Mutex<SigSet> = {
    let mut ss = SigSet::empty();
    sigprocmask(SigmaskHow::SIG_BLOCK, None, Some(&mut ss)).expect("cannot set signal mask");
    Mutex::new(ss)
  };
}

pub fn init() -> Result<()> {
  let _ = settings();

  start_sig_handler()?;

  unsafe {
    sigaction(
      Signal::SIGCHLD,
      &SigAction::new(SigHandler::SigDfl, SaFlags::empty(), SigSet::empty()),
    )?;
  }

  unsafe {
    sigaction(
      Signal::SIGUSR1,
      &SigAction::new(
        SigHandler::Handler(signal_handler),
        SaFlags::empty(),
        SigSet::empty(),
      ),
    )?;
  }

  umask(Mode::from_bits_truncate(0o022));

  if cfg!(target_os = "macos")
    && std::env::var("TMPDIR").map_or(false, |x| x.starts_with("/var/folders"))
  {
    std::env::remove_var("TMPDIR");
  }

  Ok(())
}

fn start_sig_handler() -> Result<()> {
  let _ = &*WINSIZE;

  let mut ss = SigSet::empty();
  ss.add(Signal::SIGINT);
  ss.add(Signal::SIGTERM);
  ss.add(Signal::SIGHUP);
  ss.add(Signal::SIGPIPE);
  ss.add(Signal::SIGWINCH);
  pthread_sigmask(SigmaskHow::SIG_BLOCK, Some(&ss), None)?;

  std::thread::spawn(move || loop {
    let sig = ss.wait().expect("unable to sigwait()");
    match sig {
      Signal::SIGINT | Signal::SIGTERM | Signal::SIGHUP => warn!("interrupted"),
      Signal::SIGWINCH => {
        *WINSIZE.lock().unwrap() = get_win_size();
      }
      _ => {}
    }
  });

  Ok(())
}

fn get_win_size() -> (u16, u16) {
  let mut wsz = MaybeUninit::uninit();

  let ws = unsafe {
    winsize_syscall(2, wsz.as_mut_ptr()).expect("unable to fetch window size");
    wsz.assume_init()
  };

  (ws.ws_col, ws.ws_row)
}

extern "C" fn signal_handler(sig: i32) {
  println!("received signal: {}", sig)
}
