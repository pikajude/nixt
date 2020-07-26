use std::{
  cell::UnsafeCell,
  cmp,
  fmt::{self, Debug},
  hash::{Hash, Hasher},
  marker::PhantomData,
  mem,
  ops::{Index, IndexMut},
};
mod mutex;
use mutex::Mutex;

const INITIAL_SIZE: usize = 1024;
const MIN_CAPACITY: usize = 1;

#[derive(Debug)]
struct ChunkList<T> {
  current: Vec<T>,
  rest: Vec<Vec<T>>,
}

pub struct Id<T> {
  ix0: usize,
  ix1: usize,
  _data: PhantomData<T>,
}

impl<T> Debug for Id<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Id({}:{})", self.ix0, self.ix1)
  }
}

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
  fn clone(&self) -> Self {
    *self
  }
}
impl<T> PartialEq for Id<T> {
  fn eq(&self, other: &Self) -> bool {
    self.ix0 == other.ix0 && self.ix1 == other.ix1
  }
}
impl<T> Eq for Id<T> {}
impl<T> Hash for Id<T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.ix0.hash(state);
    self.ix1.hash(state);
  }
}

/// A threadsafe allocation arena with an immutable `alloc` method.
///
/// References to items in the arena acquired via `Arena::get` or the `Index`
/// impl are guaranteed to be valid until the arena is dropped.
///
/// Concurrent `alloc` operations are guaranteed not to race.
pub struct Arena<T> {
  chunks: UnsafeCell<ChunkList<T>>,
  lock: Mutex,
}

impl<T> Default for Arena<T> {
  fn default() -> Self {
    Self::new()
  }
}

unsafe impl<T: Sync> Sync for Arena<T> {}

impl<T> Arena<T> {
  pub fn new() -> Self {
    Self::with_capacity(INITIAL_SIZE / cmp::max(1, mem::size_of::<T>()))
  }

  pub fn with_capacity(n: usize) -> Self {
    Self {
      chunks: UnsafeCell::new(ChunkList {
        current: Vec::with_capacity(cmp::max(MIN_CAPACITY, n)),
        rest: Vec::new(),
      }),
      lock: Mutex::new(),
    }
  }

  pub fn get(&self, index: Id<T>) -> Option<&T> {
    let chunks = unsafe { &*self.chunks.get() };
    if index.ix0 >= chunks.rest.len() {
      chunks.current.get(index.ix1)
    } else {
      chunks.rest.get(index.ix0)?.get(index.ix1)
    }
  }

  pub fn get_mut(&mut self, index: Id<T>) -> Option<&mut T> {
    let chunks = unsafe { &mut *self.chunks.get() };
    if index.ix0 >= chunks.rest.len() {
      chunks.current.get_mut(index.ix1)
    } else {
      chunks.rest.get_mut(index.ix0)?.get_mut(index.ix1)
    }
  }

  pub fn len(&self) -> usize {
    let chunks = unsafe { &*self.chunks.get() };
    let mut res = 0;
    for vec in chunks.rest.iter() {
      res += vec.len();
    }

    res + chunks.current.len()
  }

  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  #[inline]
  pub fn alloc(&self, item: T) -> Id<T> {
    self
      .alloc_fast(item)
      .unwrap_or_else(|v| self.alloc_extend(std::iter::once(v))[0])
  }

  #[inline]
  fn alloc_fast(&self, item: T) -> Result<Id<T>, T> {
    let _guard = self.lock.lock();
    let chunks = unsafe { &mut *self.chunks.get() };
    let len = chunks.current.len();
    if len < chunks.current.capacity() {
      chunks.current.push(item);
      Ok(Id {
        ix0: chunks.rest.len(),
        ix1: len,
        _data: PhantomData,
      })
    } else {
      Err(item)
    }
  }

  pub fn alloc_extend<I: IntoIterator<Item = T>>(&self, iterable: I) -> Vec<Id<T>> {
    let _guard = self.lock.lock();
    let mut iter = iterable.into_iter();

    let chunks = unsafe { &mut *self.chunks.get() };

    let iter_min_len = iter.size_hint().0;
    let mut next_item_index;

    debug_assert!(chunks.current.capacity() >= chunks.current.len());

    if iter_min_len > chunks.current.capacity() - chunks.current.len() {
      chunks.reserve(iter_min_len);
      chunks.current.extend(iter);
      next_item_index = 0;
    } else {
      next_item_index = chunks.current.len();
      let mut i = 0;
      while let Some(elem) = iter.next() {
        if chunks.current.len() == chunks.current.capacity() {
          chunks.reserve(i + 1);
          let previous_chunk = chunks.rest.last_mut().unwrap();
          let previous_chunk_len = previous_chunk.len();
          chunks
            .current
            .extend(previous_chunk.drain(previous_chunk_len - i..));
          chunks.current.push(elem);
          chunks.current.extend(iter);
          next_item_index = 0;
          break;
        } else {
          chunks.current.push(elem);
        }
        i += 1;
      }
    }

    let chunk_ix = chunks.rest.len();

    (next_item_index..chunks.current.len())
      .map(|x| Id {
        ix0: chunk_ix,
        ix1: x,
        _data: PhantomData,
      })
      .collect()
  }
}

impl<T> Index<Id<T>> for Arena<T> {
  type Output = T;

  fn index(&self, index: Id<T>) -> &Self::Output {
    self.get(index).expect("Invalid index")
  }
}

impl<T> IndexMut<Id<T>> for Arena<T> {
  fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
    self.get_mut(index).expect("Invalid index")
  }
}

impl<T> ChunkList<T> {
  #[inline(never)]
  #[cold]
  fn reserve(&mut self, additional: usize) {
    let double_cap = self
      .current
      .capacity()
      .checked_mul(2)
      .expect("capacity overflow");
    let required_cap = additional
      .checked_next_power_of_two()
      .expect("capacity overflow");
    let new_capacity = cmp::max(double_cap, required_cap);
    let chunk = mem::replace(&mut self.current, Vec::with_capacity(new_capacity));
    self.rest.push(chunk);
  }
}

#[test]
fn test_realloc() {
  let arr = Arena::with_capacity(1024);
  let ids1 = arr.alloc_extend(0..500);
  let item1 = &arr[ids1[0]];
  let ids2 = arr.alloc_extend(500..1000);
  let item2 = &arr[ids2[0]];
  let ids3 = arr.alloc_extend(1000..1500);
  let item3 = &arr[ids3[0]];
  let ids4 = arr.alloc_extend(1500..2000);
  let item4 = &arr[ids4[0]];
  let ids5 = arr.alloc_extend(2000..2500);
  let item5 = &arr[ids5[0]];
  let ids6 = arr.alloc_extend(2500..3000);
  let item6 = &arr[ids6[0]];
  assert_eq!(item1, &0);
  assert_eq!(item2, &500);
  assert_eq!(item3, &1000);
  assert_eq!(item4, &1500);
  assert_eq!(item5, &2000);
  assert_eq!(item6, &2500);
}
