use id_arena::Arena as A;
pub use id_arena::Id;
use std::{
  cell::UnsafeCell,
  fmt::Debug,
  ops::{Index, IndexMut},
};

/// An allocation arena type with numeric IDs. This arena disallows deletion, so
/// its `insert` method does not require mutable access to `self`, since IDs
/// cannot become invalid until the entire arena is destroyed.
#[derive(Debug)]
pub struct Arena<T>(UnsafeCell<A<T>>);

impl<T> Arena<T> {
  pub fn new() -> Self {
    Self(UnsafeCell::new(A::new()))
  }

  pub fn insert(&self, value: T) -> Id<T> {
    self.arr_mut().alloc(value)
  }

  fn arr(&self) -> &A<T> {
    unsafe { self.0.get().as_ref() }.unwrap()
  }

  #[allow(clippy::mut_from_ref)]
  fn arr_mut(&self) -> &mut A<T> {
    unsafe { self.0.get().as_mut() }.unwrap()
  }
}

impl<T> Default for Arena<T> {
  fn default() -> Self {
    Self::new()
  }
}

impl<T> Index<Id<T>> for Arena<T> {
  type Output = T;

  fn index(&self, index: Id<T>) -> &Self::Output {
    self.arr().index(index)
  }
}

impl<T> IndexMut<Id<T>> for Arena<T> {
  fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
    self.arr_mut().index_mut(index)
  }
}
