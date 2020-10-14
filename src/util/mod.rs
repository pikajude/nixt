pub mod base32;
pub use anyhow::{Context as _, *};
mod pid;
pub use pid::*;
use std::{
  io::{self, Read, Write},
  path::{Path, PathBuf},
  str::pattern::{Pattern, Searcher},
};
use unix::NixPath;

pub fn break_str<'a, P: Pattern<'a>>(s: &'a str, pattern: P) -> Option<(&'a str, &'a str)> {
  let mut search = pattern.into_searcher(s);
  let (start, end) = search.next_match()?;

  Some((&s[..start], &s[end..]))
}

#[test]
fn break_str_test() {
  assert_eq!(break_str("foobar", 'b'), Some(("foo", "ar")));
  assert_eq!(break_str("foobar", "baz"), None);
  assert_eq!(break_str("foobar", "bar"), Some(("foo", "")));
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

pub struct FnSink<F: FnMut(&[u8]) -> io::Result<()>>(F);

impl<F: FnMut(&[u8]) -> io::Result<()>> FnSink<F> {
  pub fn new(f: F) -> Self {
    Self(f)
  }
}

impl<F: FnMut(&[u8]) -> io::Result<()>> Write for FnSink<F> {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    (self.0)(buf)?;
    Ok(buf.len())
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}

pub struct SourceSink<F: FnMut(&mut dyn Write) -> io::Result<()>> {
  cb: F,
  buffer: Vec<u8>,
}

impl<F: FnMut(&mut dyn Write) -> io::Result<()>> SourceSink<F> {
  pub fn new(f: F) -> Self {
    Self {
      cb: f,
      buffer: vec![],
    }
  }
}

impl<F: FnMut(&mut dyn Write) -> io::Result<()>> Read for SourceSink<F> {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    if self.buffer.is_empty() {
      (self.cb)(&mut self.buffer)?;
    }
    let mut read_data = self
      .buffer
      .split_off(std::cmp::min(self.buffer.len(), buf.len()));
    // split_off returns the back, but i want the front here
    std::mem::swap(&mut read_data, &mut self.buffer);
    buf[..read_data.len()].copy_from_slice(&read_data);
    Ok(read_data.len())
  }
}

#[test]
fn test_sink() -> io::Result<()> {
  let mut called = false;
  let mut ss = SourceSink::new(move |writer| {
    if !called {
      writer.write_all(b"hello world!")?;
      called = true;
    }
    Ok(())
  });

  let mut output = vec![];
  std::io::copy(&mut ss, &mut output)?;
  assert_eq!(&output[..], b"hello world!");
  Ok(())
}

#[derive(Debug, AsRef, Deref, DerefMut, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[as_ref(forward)]
pub struct AutoDelete(pub PathBuf);

impl Drop for AutoDelete {
  fn drop(&mut self) {
    /*
    if self.0.to_str() == Some("/no-such-path") {
      return;
    }
    let status: Result<()> = try {
      let meta = std::fs::metadata(&self.0)?;
      if meta.is_dir() {
        std::fs::remove_dir_all(&self.0)?;
      } else {
        std::fs::remove_file(&self.0)?;
      }
    };
    if let Err(e) = status {
      debug!("failed to auto-cleanup path '{}': {}", self.0.display(), e)
    }
    */
  }
}

impl NixPath for AutoDelete {
  fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  fn len(&self) -> usize {
    self.0.len()
  }

  fn with_nix_path<T, F>(&self, f: F) -> unix::Result<T>
  where
    F: FnOnce(&std::ffi::CStr) -> T,
  {
    self.0.with_nix_path(f)
  }
}

pub fn decode_context(string: &str) -> (&Path, &str) {
  if string.starts_with('!') {
    break_str(&string[1..], '!').map_or((Path::new(&string[1..]), ""), |(a, b)| (Path::new(b), a))
  } else {
    (Path::new(string.strip_prefix('/').unwrap_or(string)), "")
  }
}

pub fn rmdir<P: AsRef<Path>>(path: P) -> io::Result<()> {
  match std::fs::remove_dir_all(path) {
    Ok(_) => Ok(()),
    Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
    Err(e) => Err(e),
  }
}
