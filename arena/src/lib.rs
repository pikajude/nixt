use generational_arena::{Arena as A, Index as I};
use std::{
  fmt::{self, Debug},
  marker::PhantomData,
  ops::{Index, IndexMut},
};

#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct Id<T> {
  index: I,
  _d: PhantomData<T>,
}

impl<T> Id<T> {
  fn id(x: I) -> Self {
    Self {
      index: x,
      _d: PhantomData,
    }
  }
}

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
  fn clone(&self) -> Self {
    Self::id(self.index)
  }
}

impl<T> Debug for Id<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.index.fmt(f)
  }
}

pub struct Arena<T>(A<T>);

impl<T> Arena<T> {
  pub fn new() -> Self {
    Self(A::new())
  }

  pub fn insert(&mut self, value: T) -> Id<T> {
    Id::id(self.0.insert(value))
  }

  pub fn get_mut(&mut self, index: Id<T>) -> Option<&mut T> {
    self.0.get_mut(index.index)
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
    self.0.index(index.index)
  }
}

impl<T> IndexMut<Id<T>> for Arena<T> {
  fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
    self.0.index_mut(index.index)
  }
}
