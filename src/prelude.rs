pub use anyhow::{anyhow, bail, ensure, Result};
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use parking_lot::RwLock;
use std::{error::Error, sync::Arc};
use string_cache::DefaultAtom;

pub type Ident = DefaultAtom;

pub type Readable<T> = Arc<T>;
pub type Writable<T> = Readable<RwLock<T>>;

pub fn readable<T>(value: T) -> Readable<T> {
  Arc::new(value)
}

pub fn writable<T>(value: T) -> Writable<T> {
  Arc::new(RwLock::new(value))
}

pub trait LocatedError: Error + Sync + Send {
  fn erased(self) -> anyhow::Error
  where
    Self: Sized + 'static,
  {
    SomeLocatedError(Box::new(self)).into()
  }

  fn diagnose(&self) -> Diagnostic<FileId> {
    unimplemented!()
  }
}

#[derive(Error, Debug)]
#[error("{0}")]
pub struct SomeLocatedError(Box<dyn LocatedError>);
