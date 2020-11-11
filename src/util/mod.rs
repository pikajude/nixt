pub use anyhow::{Context as _, *};
pub use fs::*;
pub use io::*;
pub use pid::show_status;
use std::{
  path::{Path, PathBuf},
  str::pattern::{Pattern, Searcher},
};

pub mod base32;
mod fs;
mod io;
pub mod pid;

pub fn break_str<'a, P: Pattern<'a>>(s: &'a str, pattern: P) -> Option<(&'a str, &'a str)> {
  let mut search = pattern.into_searcher(s);
  let (start, end) = search.next_match()?;

  Some((&s[..start], &s[end..]))
}

pub fn decode_context(string: &str) -> (&Path, &str) {
  if let Some(s) = string.strip_prefix('!') {
    break_str(s, '!').map_or((Path::new(&string[1..]), ""), |(a, b)| (Path::new(b), a))
  } else {
    (Path::new(string.strip_prefix('/').unwrap_or(string)), "")
  }
}

pub fn concat_paths<P: AsRef<Path>, Q: AsRef<Path>>(path1: P, path2: Q) -> PathBuf {
  let path1 = path1.as_ref();
  let path2 = path2.as_ref();
  path1.join(path2.strip_prefix("/").unwrap_or(path2))
}

pub trait OptionalExt<T> {
  fn optional(self) -> Result<Option<T>>;
}

impl<T> OptionalExt<T> for Result<T> {
  fn optional(self) -> Result<Option<T>> {
    match self {
      Ok(x) => Ok(Some(x)),
      Err(e) => {
        if e.downcast_ref() == Some(&rusqlite::Error::QueryReturnedNoRows) {
          Ok(None)
        } else {
          Err(e)
        }
      }
    }
  }
}

pub struct RunOnDrop<F: FnOnce()> {
  run: Option<F>,
}

impl<F: FnOnce()> RunOnDrop<F> {
  pub fn new(f: F) -> Self {
    Self { run: Some(f) }
  }
}

impl<F: FnOnce()> Drop for RunOnDrop<F> {
  fn drop(&mut self) {
    if let Some(t) = self.run.take() {
      t()
    }
  }
}

#[test]
fn break_str_test() {
  assert_eq!(break_str("foobar", 'b'), Some(("foo", "ar")));
  assert_eq!(break_str("foobar", "baz"), None);
  assert_eq!(break_str("foobar", "bar"), Some(("foo", "")));
}
