use self::{
  expr::{Expr, StaticEnvRef},
  lexer::{LexError, Lexer},
  parse::Located,
};
use crate::prelude::*;
use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError;
use parking_lot::Mutex;
use std::{path::Path, sync::atomic::AtomicUsize};

pub mod expr;
pub mod lexer;
pub mod parse;

static INLINE_COUNTER: AtomicUsize = AtomicUsize::new(0);

lazy_static! {
  pub(crate) static ref FILES: Mutex<Files<String>> = Mutex::new(Files::new());
}

fn parse_str(file_id: FileId, base_path: &Path, input: &str, env: &StaticEnvRef) -> Result<Expr> {
  let expr = parse::ExprParser::new()
    .parse(base_path, file_id, Lexer::new(input, file_id))
    .map_err(|e| LocatedParseError(file_id, e.map_token(|t| t.to_string())).erased())?;
  expr
    .bind_vars(env)
    .map_err(|error| LocatedParseError(file_id, ParseError::User { error }).erased())?;
  Ok(expr)
}

pub fn parse_inline(input: &str, env: &StaticEnvRef) -> Result<Expr> {
  let filename = format!(
    "<inline-{}>",
    INLINE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Acquire)
  );
  let mut files = FILES.lock();
  let id = files.add(filename, input.into());
  parse_str(
    id,
    &*std::env::current_dir().expect("no current_dir()"),
    files.source(id),
    env,
  )
}

pub fn parse_from_file<P: AsRef<Path>>(path: P, env: &StaticEnvRef) -> Result<Expr> {
  let path = path.as_ref();
  let contents = std::fs::read_to_string(path).unwrap();
  let mut files = FILES.lock();
  let id = files.add(path, contents);
  parse_str(id, path.parent().unwrap(), files.source(id), env)
}

#[derive(Debug, Error)]
pub enum UserError {
  #[error("{0}")]
  Lexer(LexError),
  #[error("{0}")]
  PathResolution(#[from] path_abs::Error),
  #[error("undefined variable {0}")]
  UndefinedVariable(Ident),
  #[error("{0}")]
  Other(String),
}

#[derive(Error, Debug)]
#[error("{0:?}")]
struct LocatedParseError(FileId, ParseError<usize, String, Located<UserError>>);

impl LocatedError for LocatedParseError {
  fn diagnose(&self) -> Diagnostic<FileId> {
    let file_id = self.0;
    let (msg, labels) = match self.1 {
      ParseError::InvalidToken { location } => (
        "invalid token".to_string(),
        vec![Label::primary(file_id, location..location)],
      ),
      ParseError::UnrecognizedEOF {
        location,
        ref expected,
      } => (
        "unexpected EOF".to_string(),
        vec![
          Label::primary(file_id, location..location).with_message(format!(
            "expected one of {}",
            itertools::join(expected, ", ")
          )),
        ],
      ),
      ParseError::UnrecognizedToken {
        ref token,
        ref expected,
      } => (
        format!("unexpected token `{}'", token.1),
        vec![
          Label::primary(file_id, token.0..token.2).with_message(format!(
            "expected one of {}",
            itertools::join(expected, ", ")
          )),
        ],
      ),
      ParseError::ExtraToken { ref token } => (
        format!("extra token in input: `{}'", token.1),
        vec![Label::primary(file_id, token.0..token.2)],
      ),
      ParseError::User { ref error } => (
        error.v.to_string(),
        vec![Label::primary(error.pos.0, error.pos.1)],
      ),
    };

    Diagnostic::error().with_labels(labels).with_message(msg)
  }
}

#[ignore]
#[test]
fn test_files() -> Result<()> {
  match parse_from_file(
    "/home/jude/.code/nix/pkgs/pkgs/top-level/all-packages.nix",
    &StaticEnvRef::default(),
  ) {
    Ok(f) => eprintln!("{:#}", f),
    Err(e) => match e.downcast::<SomeLocatedError>() {
      Ok(e) => eprintln!("{:?}", e),
      Err(other) => {
        eprintln!("{:?}", other);
        std::process::exit(1);
      }
    },
  }
  Ok(())
}
