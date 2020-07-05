use id_arena::Arena as A;
pub use id_arena::Id;
use std::{cell::UnsafeCell, fmt::Debug, ops::DerefMut};

/// An allocation arena type with numeric IDs. This arena disallows deletion, so
/// its `insert` method does not require mutable access to `self`, since IDs
/// cannot become invalid until the entire arena is destroyed.
#[derive(Debug)]
pub struct Arena<T>(UnsafeCell<A<T>>);

unsafe impl<T: Sync> Sync for Arena<T> {}

impl<T> Arena<T> {
  pub fn new() -> Self {
    Self(UnsafeCell::new(A::new()))
  }

  pub fn insert(&self, value: T) -> Id<T> {
    self.arr_mut().alloc(value)
  }

  pub fn swap(&self, id: Id<T>, value: T) -> T {
    std::mem::replace(&mut self.arr_mut()[id], value)
  }

  pub fn fetch<R: Copy, F: FnOnce(&T) -> R>(&self, id: Id<T>, cb: F) -> R {
    cb(&self.arr()[id])
  }

  pub unsafe fn index(&self, id: Id<T>) -> &T {
    &self.arr()[id]
  }

  fn arr(&self) -> &A<T> {
    unsafe { self.0.get().as_ref() }.unwrap()
  }

  #[allow(clippy::mut_from_ref)]
  fn arr_mut(&self) -> &mut A<T> {
    unsafe { self.0.get().as_mut() }.unwrap()
  }
}

impl<T: DerefMut> Arena<T>
where
  T::Target: Sized,
{
  pub fn swap_deref(&self, id: Id<T>, value: T::Target) -> T::Target {
    std::mem::replace(&mut self.arr_mut()[id].deref_mut(), value)
  }
}

impl<T> Default for Arena<T> {
  fn default() -> Self {
    Self::new()
  }
}
