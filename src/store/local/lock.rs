use crate::util::*;
use std::{
  fs::{self, File},
  os::unix::io::AsRawFd,
  path::PathBuf,
};
use unix::{
  errno::EWOULDBLOCK,
  fcntl::{self, FlockArg},
};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum LockType {
  Read,
  Write,
  Unlock,
}

impl LockType {
  fn flag(self, block: bool) -> FlockArg {
    use FlockArg::*;
    if block {
      match self {
        Self::Read => LockShared,
        Self::Write => LockExclusive,
        Self::Unlock => Unlock,
      }
    } else {
      match self {
        Self::Read => LockSharedNonblock,
        Self::Write => LockExclusiveNonblock,
        Self::Unlock => UnlockNonblock,
      }
    }
  }
}

pub trait FsExt2 {
  fn try_lock(&self, ty: LockType) -> Result<bool>;
  fn lock(&self, ty: LockType) -> Result<()>;
}

impl FsExt2 for fs::File {
  fn try_lock(&self, ty: LockType) -> Result<bool> {
    if let Err(e) = fcntl::flock(self.as_raw_fd(), ty.flag(false)) {
      if e.as_errno() == Some(EWOULDBLOCK) {
        return Ok(false);
      }
      bail!(e);
    }
    Ok(true)
  }

  fn lock(&self, ty: LockType) -> Result<()> {
    if let Err(e) = fcntl::flock(self.as_raw_fd(), ty.flag(true)) {
      bail!(e);
    }
    Ok(())
  }
}

#[derive(Default, Debug)]
pub struct PathLocks(Vec<File>);

impl PathLocks {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn lock<'a, I: IntoIterator<Item = &'a PathBuf>>(
    &mut self,
    paths: I,
    wait: bool,
    message: Option<&'static str>,
  ) -> Result<bool> {
    assert!(self.0.is_empty());
    for path in paths {
      let lock_path = match path.extension() {
        Some(e) => path.with_extension({
          let mut e = e.to_os_string();
          e.push(".lock");
          e
        }),
        None => path.with_extension(".lock"),
      };
      loop {
        let lockfile = File::create(&lock_path)?;
        if !lockfile.try_lock(LockType::Write)? {
          if wait {
            if let Some(m) = message {
              error!("{}", m);
            }
            lockfile.lock(LockType::Write)?;
          } else {
            self.unlock();
            return Ok(false);
          }
        }
        debug!("lock acquired on `{}'", lock_path.display());
        let meta = fs::metadata(&lock_path)?;
        if meta.len() != 0 {
          debug!("lock file `{}' has become stale", lock_path.display());
        } else {
          self.0.push(lockfile);
          break;
        }
      }
    }
    Ok(true)
  }

  pub fn unlock(&mut self) {
    for it in self.0.drain(..) {
      let _ = it.try_lock(LockType::Unlock);
    }
  }
}

impl Drop for PathLocks {
  fn drop(&mut self) {
    self.unlock()
  }
}
