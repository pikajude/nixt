use std::io::{prelude::*, Error, Result};

use crossbeam::{
  channel::{unbounded, Receiver, Sender},
  thread::Scope,
};

pub fn data_channel() -> (Sender<Vec<u8>>, ChannelReader) {
  let (a, b) = unbounded();
  (a, ChannelReader::new(b))
}

pub struct ChannelReader {
  receiver: Receiver<Vec<u8>>,
  here: Vec<u8>,
}

impl ChannelReader {
  pub fn new(reader: Receiver<Vec<u8>>) -> Self {
    Self {
      receiver: reader,
      here: vec![],
    }
  }
}

impl Read for ChannelReader {
  fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
    let cached_len = self.here.len();
    let target_len = buf.len();
    if cached_len >= target_len {
      buf.copy_from_slice(&self.here[..target_len]);
      self.here = self.here.split_off(target_len);
      Ok(target_len)
    } else if cached_len > 0 {
      buf[..cached_len].copy_from_slice(&self.here);
      self.here.clear();
      Ok(cached_len)
    } else {
      match self.receiver.recv() {
        Ok(bytes) => {
          if bytes.is_empty() {
            Ok(0)
          } else {
            self.here = bytes;
            self.read(buf)
          }
        }
        Err(_) => Ok(0),
      }
    }
  }
}

pub fn make_pipe<'e, R: Send + 'e, F: FnOnce(&mut dyn Write) -> R + Send + 'e>(
  scope: &Scope<'e>,
  cb: F,
) -> impl Read {
  let (sender, receiver) = crossbeam::channel::unbounded();
  scope.spawn(move |_| cb(&mut WrapWrite(sender)));
  ChannelReader::new(receiver)
}

struct WrapWrite(Sender<Vec<u8>>);

impl Write for WrapWrite {
  fn write(&mut self, buf: &[u8]) -> Result<usize> {
    self
      .0
      .send(buf.to_vec())
      .map_err(|e| Error::new(std::io::ErrorKind::Other, e))?;
    Ok(buf.len())
  }

  fn flush(&mut self) -> Result<()> {
    Ok(())
  }
}
