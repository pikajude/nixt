use im::Vector;
use std::sync::Arc;

pub trait ImmutVec<T> {
  fn prepend(&self, item: T) -> Self;
  fn append(&self, item: T) -> Self;
}

impl<T> ImmutVec<T> for Vector<Arc<T>> {
  fn prepend(&self, item: T) -> Self {
    let mut c = self.clone();
    c.push_front(Arc::new(item));
    debug!(target: "scope", "self len: {}", c.len());
    c
  }

  fn append(&self, item: T) -> Self {
    let mut c = self.clone();
    c.push_back(Arc::new(item));
    debug!(target: "scope", "self len: {}", c.len());
    c
  }
}
