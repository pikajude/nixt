use crate::prelude::Pos;
use anyhow::Result;
use codespan::FileId;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::error::Error;
use termcolor::{ColorChoice, StandardStream};

pub trait LocatedError: Error + Sync + Send {
  fn erased(self) -> anyhow::Error
  where
    Self: Sized + 'static,
  {
    SomeLocatedError(Box::new(self)).into()
  }

  fn diagnose(&self) -> Diagnostic<FileId>;
}

#[derive(Error, Debug)]
#[error("{0}")]
pub struct SomeLocatedError(pub Box<dyn LocatedError>);

#[macro_export]
macro_rules! throw {
  ($pos:expr, $e:expr) => {
    return Err(
      $crate::error::LocatedStdError {
        pos: $pos,
        err: $e.into(),
      }
      .erased(),
    );
  };
  ($pos:expr, $l:literal, $($t:tt)+) => {
    $crate::throw!($pos, format!($l, $($t)+))
  }
}

pub trait NixTerminate<T> {
  fn or_exit(self) -> T;
}

impl<T> NixTerminate<T> for Result<T> {
  fn or_exit(self) -> T {
    match self {
      Ok(v) => v,
      Err(e) => {
        match e.downcast() {
          Ok(SomeLocatedError(x)) => {
            let diag = x.diagnose();
            codespan_reporting::term::emit(
              &mut StandardStream::stderr(ColorChoice::Auto),
              &Default::default(),
              &*crate::syntax::FILES.lock(),
              &diag,
            )
            .expect("unable to output error");
          }
          Err(x) => eprintln!("{:?}", x),
        };
        std::process::exit(1)
      }
    }
  }
}

#[derive(Error, Debug)]
#[error("{err}")]
pub struct LocatedStdError {
  pub pos: Pos,
  pub err: Box<dyn std::error::Error + Send + Sync>,
}

impl LocatedError for LocatedStdError {
  fn diagnose(&self) -> Diagnostic<FileId> {
    Diagnostic::error()
      .with_labels(vec![Label::primary(self.pos.0, self.pos.1)])
      .with_message(self.to_string())
  }
}
