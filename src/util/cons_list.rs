use std::{
  iter::{FromIterator, FusedIterator},
  sync::Arc,
};

#[derive(PartialEq, Eq, Debug)]
struct Node<T> {
  value: T,
  next: Option<Arc<Node<T>>>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct ConsList<T> {
  front: Option<Arc<Node<T>>>,
  size: usize,
}

impl<T> Default for ConsList<T> {
  fn default() -> Self {
    Self::new()
  }
}

impl<T> Clone for ConsList<T> {
  fn clone(&self) -> Self {
    Self {
      front: self.front.clone(),
      size: self.size,
    }
  }
}

impl<T> ConsList<T> {
  pub fn new() -> Self {
    Self {
      front: None,
      size: 0,
    }
  }

  pub fn cons(&self, value: T) -> Self {
    let node = Node {
      value,
      next: self.front.clone(),
    };
    Self {
      front: Some(Arc::new(node)),
      size: self.size + 1,
    }
  }

  pub fn uncons(&self) -> Option<(&T, ConsList<T>)> {
    self.head().and_then(|y| Some((y, self.tail()?)))
  }

  pub fn head(&self) -> Option<&T> {
    self.front.as_ref().map(|n| &n.value)
  }

  pub fn tail(&self) -> Option<ConsList<T>> {
    self.front.as_ref().and_then(|n| {
      n.next.as_ref().map(|next| Self {
        front: Some(Arc::clone(next)),
        size: self.size - 1,
      })
    })
  }

  pub fn len(&self) -> usize {
    self.size
  }

  pub fn is_empty(&self) -> bool {
    self.size == 0
  }
}

impl<T> Drop for ConsList<T> {
  fn drop(&mut self) {
    let mut head = self.front.take();
    loop {
      let temp = head;
      match temp {
        Some(node) => match Arc::try_unwrap(node) {
          Ok(mut node) => {
            head = node.next.take();
          }
          _ => break,
        },
        _ => break,
      }
    }
  }
}

impl<V> FromIterator<V> for ConsList<V> {
  fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
    let mut s = Self::new();
    for item in iter {
      s = s.cons(item);
    }
    s
  }
}

pub struct ConsIter<'a, T> {
  l: Option<&'a Node<T>>,
  size: usize,
}

impl<'a, T> Iterator for ConsIter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    match self.l.take() {
      None => None,
      Some(n) => {
        self.l = n.next.as_deref();
        self.size -= 1;
        Some(&n.value)
      }
    }
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    (self.size, Some(self.size))
  }
}

impl<T> FusedIterator for ConsIter<'_, T> {}

impl<T> ExactSizeIterator for ConsIter<'_, T> {}

impl<'a, T> IntoIterator for &'a ConsList<T> {
  type IntoIter = ConsIter<'a, T>;
  type Item = &'a T;

  fn into_iter(self) -> Self::IntoIter {
    ConsIter {
      l: self.front.as_deref(),
      size: self.size,
    }
  }
}
