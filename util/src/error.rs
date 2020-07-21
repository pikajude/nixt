use nix_syntax::span::FileSpan;
use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Error {
  pub err: anyhow::Error,
  pub trace: Vec<FileSpan>,
}

impl Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.err.fmt(f)
  }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub trait Traced<T, E> {
  fn with_frame(self, t: FileSpan, extended_trace: bool) -> Result<T>;
}

impl<T> Traced<T, Error> for Result<T> {
  fn with_frame(self, t: FileSpan, extended_trace: bool) -> Result<T> {
    match self {
      Ok(x) => Ok(x),
      Err(mut e) => {
        if extended_trace || e.trace.is_empty() {
          e.trace.push(t);
        }
        Err(e)
      }
    }
  }
}

impl<T, E: std::error::Error + Send + Sync + 'static> Traced<T, E> for Result<T, E> {
  fn with_frame(self, t: FileSpan, _: bool) -> Result<T> {
    match self {
      Ok(x) => Ok(x),
      Err(e) => Err(Error {
        err: e.into(),
        trace: vec![t],
      }),
    }
  }
}

impl<E: Into<anyhow::Error>> From<E> for Error {
  fn from(e: E) -> Self {
    Self {
      err: e.into(),
      trace: vec![],
    }
  }
}

#[macro_export]
macro_rules! bail {
    ($msg:literal $(,)?) => {
        return Err($crate::Error::from($crate::anyhow::anyhow!($msg)));
    };
    ($err:expr $(,)?) => {
        return Err($crate::Error::from($crate::anyhow::anyhow!($err)));
    };
    ($fmt:expr, $($arg:tt)*) => {
        return Err($crate::Error::from($crate::anyhow::anyhow!($fmt, $($arg)*)));
    };
}
