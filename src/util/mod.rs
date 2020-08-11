pub mod base32;
pub use anyhow::{Context as _, *};
use std::{
  io::{self, Read, Write},
  str::pattern::{Pattern, Searcher},
};

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
