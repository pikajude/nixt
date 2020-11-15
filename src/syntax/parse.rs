pub use generated::*;
use std::{fmt, path::PathBuf};

mod generated {
  #![allow(clippy::all)]
  include!(concat!(env!("OUT_DIR"), "/syntax/parse.rs"));
}

fn homedir() -> PathBuf {
  std::env::var("HOME")
    .expect("variable $HOME not set")
    .into()
}

pub struct Located<T> {
  pub pos: Pos,
  pub v: T,
}

pub type Pos = (codespan::FileId, codespan::Span);

pub fn not_located<T>(value: T) -> Located<T> {
  Located {
    pos: (unsafe { std::mem::transmute(1) }, codespan::Span::initial()),
    v: value,
  }
}

impl<T: fmt::Debug> fmt::Debug for Located<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.v.fmt(f)
  }
}
