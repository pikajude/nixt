use codespan::FileId;
use lalrpop_util::ParseError as LALRError;

mod imp {
  #![allow(clippy::all, unused_parens)]
  include!(concat!(env!("OUT_DIR"), "/syntax/parser.rs"));
}

pub use imp::ExprParser;

#[derive(Debug, thiserror::Error)]
#[error("{perror:?}")]
pub struct ParseError {
  pub(super) id: FileId,
  pub(super) perror: LALRError<usize, String, (usize, String, usize)>,
}
