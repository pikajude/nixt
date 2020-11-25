use crate::lock::RwLock;
pub use crate::{
  atoms::Ident,
  err,
  error::*,
  syntax::parse::{not_located, Located, Pos},
  throw,
};
pub use anyhow::{anyhow, bail, ensure, Result};
use std::sync::Arc;

pub type Readable<T> = Arc<T>;
pub type Writable<T> = Readable<RwLock<T>>;

pub fn readable<T>(value: T) -> Readable<T> {
  Arc::new(value)
}

pub fn writable<T>(value: T) -> Writable<T> {
  Arc::new(RwLock::new(value))
}
