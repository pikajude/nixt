use crate::util::*;
use std::{
  borrow::Cow,
  fs::File,
  io::{self, Read, Write},
  os::unix::io::AsRawFd,
  path::{Path, PathBuf},
};
use unix::sys::stat::{fchmod, fstat, Mode};

pub struct Sink<S> {
  pub inner: S,
}

pub trait ParseSink {
  fn create_directory(&mut self, path: Option<&Path>) -> Result<()>;
  fn create_file(&mut self, path: Option<&Path>) -> Result<()>;
  fn create_symlink(&mut self, path: Option<&Path>, target: PathBuf) -> Result<()>;
  fn set_executable(&mut self) -> Result<()>;
  #[allow(unused_variables)]
  fn preallocate(&mut self, size: usize) -> Result<()> {
    Ok(())
  }
  fn receive_contents<R: Read>(&mut self, contents: R) -> Result<()>;
}

impl<S: Write> Sink<S> {
  pub fn new(inner: S) -> Self {
    Self { inner }
  }

  pub fn into_inner(self) -> S {
    self.inner
  }

  /// Write a "tag" to the inner sink. Tags are prefixed by their length and
  /// padded with zeroes until the total length is a multiple of 8.
  pub fn write_tag<B: AsRef<[u8]>>(&mut self, string: B) -> io::Result<()> {
    let string = string.as_ref();
    self.write_usize(string.len())?;
    self.write_bytes(string)?;
    self.pad(string.len())?;
    Ok(())
  }

  /// Encode a usize into the inner sink.
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

  pub fn pad(&mut self, len: usize) -> io::Result<()> {
    let rest = (len % 8) as u8;
    if rest > 0 {
      self.write_bytes(&vec![0; 8 - rest as usize])?;
    }
    Ok(())
  }

  /// Write the contents of a value implementing [Read] as a tag.
  pub fn drain<R: Read>(&mut self, mut reader: R) -> io::Result<()> {
    let len = std::io::copy(&mut reader, self)?;
    self.pad(len as _)?;
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

pub fn parse_dump<S: ParseSink, R: Read>(mut sink: S, mut source: R) -> Result<()> {
  let tag = read_bytes_len(&mut source, super::NAR_VERSION_MAGIC.len())?;
  if tag != super::NAR_VERSION_MAGIC.as_bytes() {
    bail!("input is not a valid NAR file");
  }
  parse(&mut sink, &mut source, None)
}

fn parse<S: ParseSink, R: Read>(sink: &mut S, source: &mut R, path: Option<PathBuf>) -> Result<()> {
  let buf = read_bytes(source)?;
  if buf != b"(" {
    bail!("{:?} is not a valid NAR file", buf)
  }

  #[derive(PartialEq, Eq)]
  enum EntryType {
    File,
    Dir,
    Symlink,
  }

  let mut current_ty = None;
  loop {
    match read_bytes(source)?.as_slice() {
      b")" => break,
      b"type" => {
        if current_ty.is_some() {
          bail!("trying to overwrite type when it's already set");
        }
        let tagged_type = read_bytes(source)?;
        match tagged_type.as_slice() {
          b"regular" => {
            current_ty = Some(EntryType::File);
            sink.create_file(path.as_deref())?;
          }
          b"directory" => {
            current_ty = Some(EntryType::Dir);
            sink.create_directory(path.as_deref())?;
          }
          b"symlink" => {
            current_ty = Some(EntryType::Symlink);
          }
          x => bail!("Unrecognized entry type {}", String::from_utf8_lossy(x)),
        }
      }
      b"contents" if current_ty == Some(EntryType::File) => {
        let filesize = read_num(source)?;
        sink.preallocate(filesize)?;
        sink.receive_contents(source.take(filesize as _))?;
        read_padding(source, filesize)?;
      }
      b"executable" if current_ty == Some(EntryType::File) => {
        if read_bytes(source)? != b"" {
          bail!("executable marker must be empty")
        }
        sink.set_executable()?;
      }
      b"entry" if current_ty == Some(EntryType::Dir) => {
        if read_bytes(source)? != b"(" {
          bail!("expected open tag")
        }
        let mut name = vec![];
        loop {
          let subtag = read_bytes(source)?;
          match subtag.as_slice() {
            b")" => break,
            b"name" => {
              name = read_bytes(source)?;
              if name.is_empty()
                || name == b"."
                || name == b".."
                || name.iter().any(|x| *x == b'/' || *x == 0)
              {
                bail!("invalid filename `{}'", String::from_utf8_lossy(&name))
              }
            }
            b"node" => {
              if name.is_empty() {
                bail!("expected entry name")
              }
              parse(
                sink,
                source,
                Some(match path {
                  None => PathBuf::from(String::from_utf8(name.clone())?),
                  Some(ref x) => x.join(String::from_utf8(name.clone())?),
                }),
              )?;
            }
            x => bail!("unrecognized field `{}'", String::from_utf8_lossy(x)),
          }
        }
      }
      b"target" if current_ty == Some(EntryType::Symlink) => {
        let target = read_bytes(source)?;
        sink.create_symlink(path.as_deref(), String::from_utf8(target)?.into())?;
      }
      x => bail!("unrecognized field `{}'", String::from_utf8_lossy(x)),
    }
  }

  Ok(())
}

fn read_num<R: Read>(source: &mut R) -> Result<usize> {
  let mut buf = [0u8; 8];
  source.read_exact(&mut buf)?;
  let result = buf[0] as usize
    | (buf[1] as usize) << 8
    | (buf[2] as usize) << 16
    | (buf[3] as usize) << 24
    | (buf[4] as usize) << 32
    | (buf[5] as usize) << 40
    | (buf[6] as usize) << 48
    | (buf[7] as usize) << 56;
  Ok(result)
}

fn read_padding<R: Read>(source: &mut R, len: usize) -> Result<()> {
  if len % 8 > 0 {
    let mut pad = vec![0; 8 - (len % 8)];
    source.read_exact(&mut pad)?;
    if pad.iter().any(|x| *x > 0) {
      bail!("incorrect padding");
    }
  }
  Ok(())
}

fn read_bytes_len<R: Read>(source: &mut R, max: usize) -> Result<Vec<u8>> {
  let len = read_num(source)?;
  if len > max {
    bail!("tag of length {} exceeds maximum length {}", len, max);
  }
  let mut bytes = vec![0; len];
  source.read_exact(&mut bytes)?;
  read_padding(source, len)?;
  Ok(bytes)
}

fn read_bytes<R: Read>(source: &mut R) -> Result<Vec<u8>> {
  read_bytes_len(source, usize::MAX)
}

pub struct RestoreSink {
  root: PathBuf,
  last_file: Option<File>,
}

impl RestoreSink {
  pub fn new<S: Into<PathBuf>>(s: S) -> Self {
    Self {
      root: s.into(),
      last_file: None,
    }
  }

  fn get_path(&self, p: Option<&Path>) -> Cow<Path> {
    p.map_or(Cow::Borrowed(&self.root), |p| Cow::Owned(self.root.join(p)))
  }

  fn file(&mut self) -> Result<&mut File> {
    self
      .last_file
      .as_mut()
      .ok_or_else(|| anyhow!("nar sink is in an invalid state"))
  }
}

impl ParseSink for RestoreSink {
  fn create_directory(&mut self, path: Option<&Path>) -> Result<()> {
    debug!("creating directory {}", self.get_path(path).display());
    if self.get_path(path).exists() {
      return Ok(());
    }
    Ok(std::fs::create_dir(self.get_path(path))?)
  }

  fn create_file(&mut self, path: Option<&Path>) -> Result<()> {
    debug!("importing file {}", self.get_path(path).display());
    self.last_file = Some(File::create(self.get_path(path))?);
    Ok(())
  }

  #[allow(unused)]
  fn create_symlink(&mut self, path: Option<&Path>, target: PathBuf) -> Result<()> {
    todo!()
  }

  fn set_executable(&mut self) -> Result<()> {
    let file = self.file()?;
    let stat = fstat(file.as_raw_fd())?;
    fchmod(
      file.as_raw_fd(),
      Mode::from_bits_truncate(stat.st_mode) | Mode::S_IXUSR | Mode::S_IXGRP | Mode::S_IXOTH,
    )?;
    Ok(())
  }

  #[cfg(target_os = "linux")]
  fn preallocate(&mut self, size: usize) -> Result<()> {
    debug!("preallocating {} bytes", size);
    unix::fcntl::posix_fallocate(self.file()?.as_raw_fd(), 0, size as i64)?;
    Ok(())
  }

  fn receive_contents<R: Read>(&mut self, mut contents: R) -> Result<()> {
    let bytes = std::io::copy(&mut contents, self.file()?)?;
    debug!("imported {} bytes", bytes);
    Ok(())
  }
}
