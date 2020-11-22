pub use generated::*;
use std::{
  fmt::{self, Debug, Display, Formatter},
  path::PathBuf,
};

mod generated {
  #![allow(clippy::all)]
  include!(concat!(env!("OUT_DIR"), "/syntax/parse.rs"));
}

fn homedir() -> PathBuf {
  std::env::var("HOME")
    .expect("variable $HOME not set")
    .into()
}

#[derive(Clone)]
pub struct Located<T> {
  pub pos: Pos,
  pub v: T,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Copy, Hash)]
pub struct Pos(pub codespan::FileId, pub codespan::Span);

impl Debug for Pos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Pos({})", self)
  }
}

impl Display for Pos {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let files = super::FILES.lock();
    let filename = files.name(self.0);
    let loc1 = files.location(self.0, self.1.start()).unwrap();
    let loc2 = files.location(self.0, self.1.end()).unwrap();
    write!(
      f,
      "{}:{}:{}-{}:{}",
      filename.to_string_lossy(),
      loc1.line.number(),
      loc1.column.number(),
      loc2.line.number(),
      loc2.column.number()
    )
  }
}

impl Pos {
  pub fn none() -> Self {
    Self(unsafe { std::mem::transmute(1) }, codespan::Span::initial())
  }
}

pub fn not_located<T>(value: T) -> Located<T> {
  Located {
    pos: Pos::none(),
    v: value,
  }
}

impl<T: fmt::Debug> fmt::Debug for Located<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.v.fmt(f)
  }
}
