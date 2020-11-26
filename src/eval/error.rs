use crate::prelude::*;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;

#[derive(Debug, Error, Clone)]
pub enum Catchable {
  #[error("assertion failed")]
  AssertionFailure(Pos),
  #[error("{0}")]
  ThrownError(Pos, String),
}

impl Catchable {
  fn pos(&self) -> Pos {
    match self {
      Self::AssertionFailure(p) | Self::ThrownError(p, _) => *p,
    }
  }
}

impl LocatedError for Catchable {
  fn diagnose(&self) -> Diagnostic<FileId> {
    LocatedStdError {
      pos: self.pos(),
      err: self.clone().into(),
    }
    .diagnose()
  }
}
