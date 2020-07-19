use im::Vector;

pub trait ImmutVec<T> {
  fn prepend(&self, item: T) -> Self;
}

impl<T: Clone> ImmutVec<T> for Vector<T> {
  fn prepend(&self, item: T) -> Self {
    let mut c = self.clone();
    c.push_front(item);
    c
  }
}
