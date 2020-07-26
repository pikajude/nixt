use std::io::{self, Write};

pub(super) struct Sink<S> {
  pub inner: S,
}

impl<S: Write> Sink<S> {
  pub fn write_str<B: AsRef<str>>(&mut self, string: B) -> io::Result<()> {
    let string = string.as_ref();
    self.write_usize(string.len())?;
    self.write_bytes(string.as_bytes())?;
    self.pad(string.len())?;
    Ok(())
  }

  pub fn write_usize(&mut self, len: usize) -> io::Result<()> {
    let mut buf = [0u8; 8];
    buf[0] = len as u8;
    buf[1] = (len >> 8) as u8;
    buf[2] = (len >> 16) as u8;
    buf[3] = (len >> 24) as u8;
    buf[4] = (len >> 32) as u8;
    buf[5] = (len >> 40) as u8;
    buf[6] = (len >> 48) as u8;
    buf[7] = (len >> 56) as u8;
    self.write_bytes(buf)
  }

  pub fn write_bytes<B: AsRef<[u8]>>(&mut self, bytes: B) -> io::Result<()> {
    self.inner.write_all(bytes.as_ref())
  }

  fn pad(&mut self, len: usize) -> io::Result<()> {
    let rest = (len % 8) as u8;
    if rest > 0 {
      self.write_bytes(&vec![0; 8 - rest as usize])?;
    }
    Ok(())
  }
}

impl<W: Write> Write for Sink<W> {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.inner.write(buf)
  }

  fn flush(&mut self) -> io::Result<()> {
    self.inner.flush()
  }
}
