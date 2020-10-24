use crate::prelude::*;
pub use scan::RefsScanner;
pub use sink::Sink;
use std::io::{Read, Write};
use tee_readwrite::{TeeReader, TeeWriter};
use unix::sys::stat::Mode;

mod scan;
mod sink;

pub struct PathFilter(Option<Box<dyn Fn(&Path) -> bool + Send + Sync>>);

impl PathFilter {
  pub fn none() -> Self {
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

pub fn dump_path<P: AsRef<Path>, W: Write>(
  path: P,
  sink: W,
  filter: &PathFilter,
) -> io::Result<()> {
  let mut s = Sink { inner: sink };
  s.write_tag(NAR_VERSION_MAGIC)?;
  dump_path_impl(path, &mut s, filter)
}

fn dump_path_impl<P: AsRef<Path>, W: Write>(
  path: P,
  sink: &mut Sink<W>,
  filter: &PathFilter,
) -> io::Result<()> {
  let path = path.as_ref();
  let meta = fs::symlink_metadata(path)?;
  sink.write_tag("(")?;
  if meta.file_type().is_file() {
    trace!("dump file: {}", path.display());
    sink.write_tag("type")?;
    sink.write_tag("regular")?;
    if Mode::from_bits_truncate(meta.mode() as _).contains(Mode::S_IXUSR) {
      sink.write_tag("executable")?;
      sink.write_tag("")?;
    }

    dump_file(path, meta.len(), sink)?;
  } else if meta.file_type().is_dir() {
    trace!("dump directory: {}", path.display());
    sink.write_tag("type")?;
    sink.write_tag("directory")?;

    for file in fs::read_dir(path)? {
      let file = file?;
      if filter(&file.path()) {
        sink.write_tag("entry")?;
        sink.write_tag("(")?;
        sink.write_tag("name")?;

        let name = file.file_name();
        sink.write_tag(
          name
            .to_str()
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Invalid filename"))?,
        )?;

        sink.write_tag("node")?;
        dump_path_impl(&file.path(), sink, filter)?;
        sink.write_tag(")")?;
      }
    }
  } else if meta.file_type().is_symlink() {
    trace!("dump symlink: {}", path.display());
    sink.write_tag("type")?;
    sink.write_tag("symlink")?;
    sink.write_tag("target")?;
    sink.write_tag(std::fs::read_link(path)?.display().to_string())?;
  } else {
    return Err(io::Error::new(
      io::ErrorKind::Other,
      format!("path `{}' has an unsupported type", path.display()),
    ));
  }

  sink.write_tag(")")?;

  Ok(())
}

fn dump_file<P: AsRef<Path>, W: Write>(path: P, size: u64, sink: &mut Sink<W>) -> io::Result<()> {
  sink.write_tag("contents")?;
  sink.write_usize(size as usize)?;

  sink.drain(fs::File::open(path)?)?;

  Ok(())
}

pub fn hash_path<P: AsRef<Path>>(
  path: P,
  ty: HashType,
  filter: &PathFilter,
) -> io::Result<(Hash, usize)> {
  let mut sink = Sink {
    inner: super::hash::Sink::new(ty),
  };
  dump_path(path, &mut sink, filter)?;
  Ok(sink.inner.finish())
}

pub static NAR_VERSION_MAGIC: &str = "nix-archive-1";

pub fn dump_contents<S: io::Read, K: io::Write>(size: usize, reader: S, sink: K) -> io::Result<()> {
  dump_contents_impl(size, reader, &mut Sink { inner: sink })
}

pub struct DumpResult {
  pub contents_hash: Hash,
  pub nar_hash: Hash,
  pub nar_size: usize,
}

pub fn dump_contents_hash<S: io::Read, K: io::Write>(
  size: usize,
  string: S,
  sink: K,
  ty: HashType,
) -> io::Result<DumpResult> {
  let mut contents_hash = crate::hash::Sink::new(ty);
  let mut nar_hash = crate::hash::Sink::new(ty);
  dump_contents_impl(
    size,
    TeeReader::new(string, &mut contents_hash, false),
    &mut Sink {
      inner: TeeWriter::new(sink, &mut nar_hash),
    },
  )?;
  let (n, u) = nar_hash.finish();
  Ok(DumpResult {
    contents_hash: contents_hash.finish().0,
    nar_hash: n,
    nar_size: u,
  })
}

fn dump_contents_impl<S: io::Read, K: io::Write>(
  size: usize,
  string: S,
  sink: &mut Sink<K>,
) -> io::Result<()> {
  sink.write_tag(NAR_VERSION_MAGIC)?;
  sink.write_tag("(")?;
  sink.write_tag("type")?;
  sink.write_tag("regular")?;
  sink.write_tag("contents")?;
  sink.write_usize(size)?;
  sink.drain(string)?;
  sink.write_tag(")")
}

pub fn dump_to_bytes<S: io::Read>(size: usize, string: S) -> io::Result<Vec<u8>> {
  let mut s = Vec::new();
  dump_contents_impl(size, string, &mut Sink { inner: &mut s })?;
  Ok(s)
}

pub fn restore_path<P: AsRef<Path>, R: Read>(dest: P, source: R) -> Result<()> {
  sink::parse_dump(sink::RestoreSink::new(dest.as_ref()), source)
}
