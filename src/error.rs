use anyhow::Result;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
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
