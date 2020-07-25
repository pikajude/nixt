use super::StorePath;
use crate::{
  hash::{Hash, HashType},
  Store,
};
use lock::{FsExt2, LockType};
use nix_util::*;
use std::{borrow::Cow, ffi::OsStr, fs, io::Write, path::PathBuf};

mod gc;
mod lock;

pub struct LocalStore {
  store_dir: PathBuf,
  state_dir: PathBuf,
  temproots_dir: PathBuf,
}

impl Store for LocalStore {
  fn store_path(&self) -> Cow<OsStr> {
    Cow::Borrowed(self.store_dir.as_os_str())
  }

  fn add_text_to_store<'a>(
    &self,
    name: &str,
    contents: &str,
    references: &mut dyn Iterator<Item = &'a StorePath>,
    repair: bool,
  ) -> Result<StorePath> {
    let hash = Hash::hash_str(contents, HashType::SHA256);
    let dest_path = self.make_text_path(name, &hash, references)?;

    self.add_temp_root(&dest_path)?;

    if repair {}

    todo!("{}", self.print_store_path(&dest_path));
  }
}

impl LocalStore {
  pub fn open() -> Result<Self> {
    let root = concat!(env!("OUT_DIR"), "/nix");
    let root_dir = PathBuf::from(root);
    let this = Self {
      store_dir: root_dir.join("store"),
      state_dir: root_dir.join("var").join("nix"),
      temproots_dir: root_dir.join("var").join("nix").join("gcroots"),
    };
    fs::create_dir_all(&this.store_dir)?;
    fs::create_dir_all(&this.temproots_dir)?;
    Ok(this)
  }

  fn add_temp_root(&self, path: &StorePath) -> Result<()> {
    let file = self.temproots_dir.join(std::process::id().to_string());
    let mut temp_file = loop {
      let all_gc_roots = gc::open_gc_lock(&self.state_dir, LockType::Read)?;
      let _ = fs::remove_file(&file);
      let temproots_file = fs::OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(&file)?;
      drop(all_gc_roots);
      debug!("acquiring read lock on `{}'", file.display());
      temproots_file.lock(LockType::Read)?;
      if temproots_file.metadata()?.len() == 0 {
        break temproots_file;
      }
    };
    debug!("acquiring write lock on `{}'", file.display());
    temp_file.lock(LockType::Write)?;
    temp_file.write_all(self.print_store_path(path).as_bytes())?;
    temp_file.lock(LockType::Read)?;
    Ok(())
  }
}
