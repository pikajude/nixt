use crate::value::Typename;
use std::fmt::Debug;
use syntax::span::FileSpan;

#[derive(thiserror::Error, Debug)]
pub enum ErrorKind {
  #[error("unbound variable: `{0}'")]
  UnboundVar(String),
  #[error(
    "this expression appears on the LHS of an application, but it is {0}, not a lambda or functor"
  )]
  NotCallable(Typename),
  #[error("this function cannot be auto-called")]
  Autocall,
  #[error("not yet implemented: {0}")]
  Unimplemented(String),
  #[error(transparent)]
  Custom(#[from] anyhow::Error),
}

#[derive(thiserror::Error, Debug)]
#[error("{kind}")]
pub struct Error {
  pub kind: ErrorKind,
  pub trace: Vec<FileSpan>,
}

impl Error {
  pub fn add_frame(mut self, f: FileSpan) -> Self {
    self.trace.push(f);
    self
  }
}

impl<T> From<T> for Error
where
  ErrorKind: From<T>,
{
  fn from(k: T) -> Self {
    Self {
      kind: ErrorKind::from(k),
      trace: vec![],
    }
  }
}

#[macro_export]
macro_rules! todo {
  ($($t:tt)+) => {
    return Err(ErrorKind::Unimplemented(format!($($t)+)).into());
  }
}
