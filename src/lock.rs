use parking_lot::{RwLock as Lock, RwLockReadGuard, RwLockWriteGuard};
use std::path::Path;

#[cfg(not(feature = "trace-locks"))]
macro_rules! trace {
  ($($t:tt)*) => {};
}

#[derive(Debug, Default)]
pub struct RwLock<T>(Lock<T>);

impl<T> RwLock<T> {
  pub fn new(value: T) -> Self {
    Self(Lock::new(value))
  }

  pub fn read(&self) -> RwLockReadGuard<T> {
    trace!("acquiring read lock on {:p}", self);
    match self.0.try_read() {
      Some(g) => g,
      None => {
        error!("lock already held on {:p}", self);
        panic!()
      }
    }
  }

  pub fn data_ptr(&self) -> *mut T {
    self.0.data_ptr()
  }

  pub fn write(&self) -> RwLockWriteGuard<T> {
    trace!("acquiring write lock on {:p}", self);
    let mut frames = 0;
    backtrace::trace(|frame| {
      frames += 1;

      backtrace::resolve_frame(frame, |sym| {
        trace!(
          "trace: {}:{:?}",
          sym
            .filename()
            .unwrap_or_else(|| Path::new("<unknown>"))
            .display(),
          sym.lineno().unwrap_or(0)
        );
      });

      frames <= 5
    });
    match self.0.try_write() {
      Some(g) => g,
      None => {
        error!("lock already held on {:p}", self);
        panic!()
      }
    }
  }
}
