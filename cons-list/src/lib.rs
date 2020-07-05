use std::{fmt, sync::Arc};

struct Node<T> {
  elem: T,
  next: Option<Arc<Node<T>>>,
}

pub struct Iter<'a, T: 'a> {
  head: Option<&'a Node<T>>,
  nelem: usize,
}

impl<'a, T> Iterator for Iter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    let head = self.head.take()?;
    self.nelem -= 1;
    self.head = head.next.as_deref();
    Some(&head.elem)
  }
}

pub struct ConsList<T> {
  front: Option<Arc<Node<T>>>,
  length: usize,
}

impl<T> ConsList<T> {
  pub fn new() -> Self {
    Self {
      front: None,
      length: 0,
    }
  }

  pub fn iter(&self) -> Iter<T> {
    Iter {
      head: self.front.as_deref(),
      nelem: self.len(),
    }
  }

  pub fn append(&self, elem: T) -> ConsList<T> {
    let mut new_node = Node { elem, next: None };
    new_node.next = self.front.clone();

    ConsList {
      front: Some(Arc::new(new_node)),
      length: self.len() + 1,
    }
  }

  pub fn len(&self) -> usize {
    self.length
  }

  pub fn is_empty(&self) -> bool {
    self.length == 0
  }
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
      length: self.length,
    }
  }
}

impl<'a, T> IntoIterator for &'a ConsList<T> {
  type IntoIter = Iter<'a, T>;
  type Item = &'a T;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

impl<T: fmt::Debug> fmt::Debug for ConsList<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("[")?;
    for (i, e) in self.iter().enumerate() {
      if i > 0 {
        f.write_str(", ")?;
      }
      write!(f, "{:?}", e)?;
    }
    write!(f, "]")
  }
}
