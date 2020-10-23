use std::io::{prelude::*, Result};

pub struct FnSink<F: FnMut(&[u8]) -> Result<()>>(F);

impl<F: FnMut(&[u8]) -> Result<()>> FnSink<F> {
  pub fn new(f: F) -> Self {
    Self(f)
  }
}

impl<F: FnMut(&[u8]) -> Result<()>> Write for FnSink<F> {
  fn write(&mut self, buf: &[u8]) -> Result<usize> {
    (self.0)(buf)?;
    Ok(buf.len())
  }

  fn flush(&mut self) -> Result<()> {
    Ok(())
  }
}

pub struct SourceSink<F: FnMut(&mut dyn Write) -> Result<()>> {
  cb: F,
  buffer: Vec<u8>,
}

impl<F: FnMut(&mut dyn Write) -> Result<()>> SourceSink<F> {
  pub fn new(f: F) -> Self {
    Self {
      cb: f,
      buffer: vec![],
    }
  }
}

impl<F: FnMut(&mut dyn Write) -> Result<()>> Read for SourceSink<F> {
  fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
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
fn test_sink() -> Result<()> {
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