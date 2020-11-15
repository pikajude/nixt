use self::{
  expr::Expr,
  lexer::{LexError, Lexer},
  parse::Located,
};
use crate::prelude::*;
use codespan::{FileId, Files};
use lalrpop_util::ParseError;
use parking_lot::Mutex;
use std::{path::Path, sync::atomic::AtomicUsize};

pub mod expr;
pub mod lexer;
pub mod parse;

static INLINE_COUNTER: AtomicUsize = AtomicUsize::new(0);

lazy_static! {
  static ref FILES: Mutex<Files<String>> = Mutex::new(Files::new());
}

fn parse_str(file_id: FileId, base_path: &Path, input: &str) -> Result<Expr> {
  parse::ExprParser::new()
    .parse(base_path, file_id, Lexer::new(input, file_id))
    .map_err(|e| LocatedParseError(e.map_token(|t| t.to_string())).erased())
}

pub fn parse_inline(input: &str) -> Result<Expr> {
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
  )
}

pub fn parse_from_file<P: AsRef<Path>>(path: P) -> Result<Expr> {
  let path = path.as_ref();
  let contents = std::fs::read_to_string(path).unwrap();
  let mut files = FILES.lock();
  let id = files.add(path, contents);
  parse_str(id, path.parent().unwrap(), files.source(id))
}

#[derive(Debug, Error)]
pub enum UserError {
  #[error("{0}")]
  Lexer(LexError),
  #[error("{0}")]
  PathResolution(#[from] path_abs::Error),
  #[error("{0}")]
  Other(String),
}

#[derive(Error, Debug)]
#[error("{0:?}")]
struct LocatedParseError(ParseError<usize, String, Located<UserError>>);

impl LocatedError for LocatedParseError {}

#[test]
fn test_files() -> Result<()> {
  match parse_from_file("/home/jude/.code/nix/pkgs/pkgs/top-level/all-packages.nix") {
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
