use parking_lot::{lock_api::RawMutex as _, RawMutex};

pub struct Mutex {
  inner: RawMutex,
}

impl Mutex {
  pub const fn new() -> Mutex {
    Mutex {
      inner: RawMutex::INIT,
    }
  }

  pub fn lock(&self) -> MutexGuard<'_> {
    self.inner.lock();
    MutexGuard { inner: &self.inner }
  }
}

pub struct MutexGuard<'a> {
  inner: &'a RawMutex,
}

impl Drop for MutexGuard<'_> {
  fn drop(&mut self) {
    unsafe {
      self.inner.unlock();
    }
  }
}
