use crate::hash::{Hash, HashType};
use nix_util::*;
use sink::Sink;
use std::{
  fs,
  io::{self, Write},
  os::unix::fs::MetadataExt,
  path::Path,
};
use unix::sys::stat::Mode;
mod sink;

pub struct PathFilter(Option<Box<dyn Fn(&Path) -> bool>>);

impl PathFilter {
  pub fn no_filter() -> Self {
    Self(None)
  }
}

impl FnOnce<(&Path,)> for PathFilter {
  type Output = bool;

  extern "rust-call" fn call_once(self, args: (&Path,)) -> Self::Output {
    if let Some(x) = self.0 {
      x.call_once(args)
    } else {
      true
    }
  }
}

impl FnMut<(&Path,)> for PathFilter {
  extern "rust-call" fn call_mut(&mut self, args: (&Path,)) -> Self::Output {
    if let Some(x) = &self.0 {
      x.call_once(args)
    } else {
      true
    }
  }
}

impl Fn<(&Path,)> for PathFilter {
  extern "rust-call" fn call(&self, args: (&Path,)) -> Self::Output {
    if let Some(x) = &self.0 {
      x.call_once(args)
    } else {
      true
    }
  }
}

pub fn dump_path<P: AsRef<Path>, W: Write>(path: P, sink: W, filter: &PathFilter) -> Result<()> {
  dump_path_impl(path, &mut Sink { inner: sink }, filter)
}

fn dump_path_impl<P: AsRef<Path>, W: Write>(
  path: P,
  sink: &mut Sink<W>,
  filter: &PathFilter,
) -> Result<()> {
  let path = path.as_ref();
  let meta = fs::metadata(path)?;
  sink.write_str("(")?;
  if meta.file_type().is_file() {
    sink.write_str("type")?;
    sink.write_str("regular")?;
    if Mode::from_bits_truncate(meta.mode() as _).contains(Mode::S_IXUSR) {
      sink.write_str("executable")?;
      sink.write_str("")?;
    }

    dump_file(path, meta.len(), sink)?;
  } else if meta.file_type().is_dir() {
    sink.write_str("type")?;
    sink.write_str("directory")?;

    for file in fs::read_dir(path)? {
      let file = file?;
      if filter(&file.path()) {
        sink.write_str("entry")?;
        sink.write_str("(")?;
        sink.write_str("name")?;

        let name = file.file_name();
        sink.write_str(
          name
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Invalid filename"))?,
        )?;

        sink.write_str("node")?;
        dump_path_impl(&file.path(), sink, filter)?;
        sink.write_str(")")?;
      }
    }
  } else if meta.file_type().is_symlink() {
    sink.write_str("type")?;
    sink.write_str("symlink")?;
    sink.write_str("target")?;
    sink.write_str(fs::canonicalize(path)?.display().to_string())?;
  } else {
    bail!("path `{}' has an unsupported type", path.display());
  }

  sink.write_str(")")?;

  Ok(())
}

fn dump_file<P: AsRef<Path>, W: Write>(path: P, size: u64, sink: &mut Sink<W>) -> Result<()> {
  sink.write_str("contents")?;
  sink.write_usize(size as usize)?;

  std::io::copy(&mut fs::File::open(path)?, sink)?;

  if size % 8 > 0 {
    sink.write_bytes(vec![0u8; 8 - (size as usize % 8)])?;
  }

  Ok(())
}

pub fn hash_path<P: AsRef<Path>>(
  path: P,
  ty: HashType,
  filter: &PathFilter,
) -> Result<(Hash, usize)> {
  let mut sink = Sink {
    inner: crate::hash::Sink::new(ty),
  };
  dump_path(path, &mut sink, filter)?;
  Ok(sink.inner.finish())
}

static NAR_VERSION_MAGIC: &str = "nix-archive-1";

pub fn dump_string<S: AsRef<str>, K: io::Write>(string: S, sink: K) -> io::Result<()> {
  dump_string_impl(string, &mut Sink { inner: sink })
}

fn dump_string_impl<S: AsRef<str>, K: io::Write>(string: S, sink: &mut Sink<K>) -> io::Result<()> {
  sink.write_str(NAR_VERSION_MAGIC)?;
  sink.write_str("(")?;
  sink.write_str("type")?;
  sink.write_str("regular")?;
  sink.write_str("contents")?;
  sink.write_str(string.as_ref())?;
  sink.write_str(")")
}

pub fn dump_to_bytes<S: AsRef<str>>(string: S) -> io::Result<Vec<u8>> {
  let mut s = Vec::new();
  dump_string_impl(string, &mut Sink { inner: &mut s })?;
  Ok(s)
}
