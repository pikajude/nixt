use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError as LALRError;

mod imp {
  #![allow(clippy::all)]
  include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

pub use imp::ExprParser;

#[derive(Debug, thiserror::Error)]
#[error("{perror:?}")]
pub struct ParseError {
  pub id: FileId,
  pub perror: LALRError<usize, String, (usize, String, usize)>,
}

impl ParseError {
  pub fn diagnose(&self) -> Diagnostic<FileId> {
    use self::LALRError::*;
    let fid = self.id;
    Diagnostic::error()
      .with_code("PARSE")
      .with_labels(vec![match self.perror {
        InvalidToken { location } => {
          Label::primary(fid, Span::new(location as u32, location as u32))
            .with_message("Invalid token")
        }
        UnrecognizedEOF {
          location,
          ref expected,
        } => Label::primary(fid, Span::new(location as u32, location as u32))
          .with_message(format!("Unexpected EOF: expected {}", expected.join(", "))),
        UnrecognizedToken {
          ref token,
          ref expected,
        } => Label::primary(fid, Span::new(token.0 as u32, token.2 as u32)).with_message(format!(
          "Unexpected token {}: expected {}",
          token.1,
          expected.join(", ")
        )),
        ExtraToken { ref token } => Label::primary(fid, Span::new(token.0 as u32, token.2 as u32))
          .with_message(format!("Extra token at end of input: {}", token.2)),
        User {
          error: (loc1, ref msg, loc2),
        } => Label::primary(fid, Span::new(loc1 as u32, loc2 as u32)).with_message(msg),
      }])
  }
}
