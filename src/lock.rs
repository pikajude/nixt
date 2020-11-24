use parking_lot::{RwLock as Lock, RwLockReadGuard as ReadGuard, RwLockWriteGuard as WriteGuard};
use std::ops::{Deref, DerefMut};

#[derive(Debug)]
pub struct RwLockReadGuard<'a, T>(ReadGuard<'a, T>);

impl<'a, T> Deref for RwLockReadGuard<'a, T> {
  type Target = <ReadGuard<'a, T> as Deref>::Target;

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<'a, T> Drop for RwLockReadGuard<'a, T> {
  fn drop(&mut self) {
    // trace!("releasing read lock on {:p}", ReadGuard::rwlock(&self.0));
  }
}

#[derive(Debug)]
pub struct RwLockWriteGuard<'a, T>(WriteGuard<'a, T>);

impl<'a, T> Deref for RwLockWriteGuard<'a, T> {
  type Target = <WriteGuard<'a, T> as Deref>::Target;

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

impl<'a, T> DerefMut for RwLockWriteGuard<'a, T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    self.0.deref_mut()
  }
}

impl<'a, T> Drop for RwLockWriteGuard<'a, T> {
  fn drop(&mut self) {
    // trace!("releasing write lock on {:p}", WriteGuard::rwlock(&self.0));
  }
}

#[derive(Debug, Default)]
pub struct RwLock<T>(Lock<T>);

impl<T> RwLock<T> {
  pub fn new(value: T) -> Self {
    Self(Lock::new(value))
  }

  pub fn read(&self) -> RwLockReadGuard<T> {
    // trace!("acquiring read lock on {:p}", self);
    match self.0.try_read() {
      Some(g) => RwLockReadGuard(g),
      None => {
        error!("lock already held on {:p}", self);
        panic!()
      }
    }
  }

  pub fn write(&self) -> RwLockWriteGuard<T> {
    // trace!("acquiring write lock on {:p}", self);
    match self.0.try_write() {
      Some(g) => RwLockWriteGuard(g),
      None => {
        error!("lock already held on {:p}", self);
        panic!()
      }
    }
  }
}
