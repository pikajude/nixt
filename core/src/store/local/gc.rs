use super::lock::{FsExt2, LockType};
use nix_util::*;
use std::{fs::File, path::Path};

pub fn open_gc_lock<P: AsRef<Path>>(state_dir: P, l: LockType) -> Result<File> {
  let gc_lock = state_dir.as_ref().join("gc.lock");
  debug!("acquiring global GC lock at `{}'", gc_lock.display());
  let f = File::create(&gc_lock)?;
  if f.try_lock(l).is_err() {
    info!("waiting for the big GC lock at `{}'...", gc_lock.display());
    f.lock(l)?;
  }
  Ok(f)
}
