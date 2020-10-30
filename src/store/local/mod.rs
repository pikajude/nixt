use super::{CheckSigsFlag, ClosureOpts, FileIngestionMethod, RepairFlag};
use crate::{archive, build::Worker, prelude::*, sqlite::Sqlite, sync::fs_lock::*};
use archive::PathFilter;
use fs::File;
use parking_lot::Mutex;
use std::{
  borrow::Cow,
  collections::BTreeSet,
  ffi::OsStr,
  fs,
  io::Write,
  iter,
  os::unix::io::AsRawFd,
  path::{Path, PathBuf},
  rc::Rc,
  sync::Arc,
};
use tee_readwrite::TeeWriter;
use unix::unistd::*;

pub mod db;
pub mod gc;

#[derive(Default)]
pub struct OptimiseStats {
  pub files_linked: u32,
  pub bytes_freed: u64,
  pub blocks_freed: u64,
}

#[derive(Debug)]
pub struct LocalStore {
  temproots_dir: PathBuf,
  links_dir: PathBuf,
  db: Mutex<Sqlite>,
}

impl Store for LocalStore {
  fn store_path(&self) -> Cow<OsStr> {
    Cow::Borrowed(settings().paths.nix_store.as_os_str())
  }

  fn add_text_to_store<'a>(
    &self,
    name: &str,
    contents: &str,
    references: &mut dyn Iterator<Item = &'a StorePath>,
    repair: RepairFlag,
  ) -> Result<StorePath> {
    let repair = repair == RepairFlag::Repair;
    let hash = Hash::hash_str(contents, HashType::SHA256);
    let dest_path = self.make_text_path(name, &hash, references)?;

    self.add_temp_root(&dest_path)?;

    if repair || !self.is_valid_path(&dest_path)? {
      let real_path = self.to_real_path(&dest_path)?;

      let _ = PathLocks::new().lock(iter::once(&real_path), true, None)?;

      if repair || !self.is_valid_path(&dest_path)? {
        delete_path(&real_path)?;

        fs::write(&real_path, contents)?;

        debug!("added {} to store", real_path.display());

        canonicalise_path_metadata(&real_path, None)?;

        let nar_bytes = archive::dump_to_bytes(contents.len(), contents.as_bytes())?;
        let nar_hash = Hash::hash_bytes(&nar_bytes, HashType::SHA256);

        optimise_path(&real_path)?;

        let mut path_info = ValidPathInfo::new(dest_path.clone(), nar_hash);
        path_info.nar_size = Some(nar_bytes.len() as u64);
        path_info.references = references.cloned().collect();

        self.register_valid_path(path_info)?;
      }
    }

    Ok(dest_path)
  }

  fn get_path_info(&self, path: &StorePath) -> Result<Option<Rc<dyn PathInfo>>> {
    let conn = self.db.lock();
    if let Some(x) = db::get_path_info(&conn, self, path)? {
      Ok(Some(Rc::new(x)))
    } else {
      Ok(None)
    }
  }

  fn add_temp_root(&self, path: &StorePath) -> Result<()> {
    let file = self.temproots_dir.join(std::process::id().to_string());
    let mut temp_file = loop {
      let all_gc_roots = gc::open_gc_lock(&settings().paths.nix_state_dir, LockType::Read)?;
      delete_path(&file)?;
      let temproots_file = File::create(&file)?;
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

  fn register_valid_path(&self, path_info: ValidPathInfo) -> Result<()> {
    let mut sql = self.db.lock();
    db::insert_valid_paths(&mut sql, self, iter::once(&path_info))?;
    Ok(())
  }

  fn add_to_store_from_source(
    &self,
    path_info: &dyn PathInfo,
    input: &mut dyn std::io::Read,
    repair: RepairFlag,
    _check_sigs: CheckSigsFlag,
  ) -> Result<()> {
    self.add_temp_root(path_info.store_path())?;

    if repair.repair() || !self.is_valid_path(path_info.store_path())? {
      let dest = self.to_real_path(path_info.store_path())?;

      PathLocks::new().lock(&mut iter::once(&dest), true, None)?;

      if repair.repair() || !self.is_valid_path(path_info.store_path())? {
        delete_path(&dest)?;

        archive::restore_path(&dest, input)?;

        canonicalise_path_metadata(&dest, None)?;
      }
    }

    Ok(())
  }

  fn add_to_store_from_path(
    &self,
    name: &str,
    path: &Path,
    ingest_method: FileIngestionMethod,
    hash_type: HashType,
    filter: &PathFilter,
    repair: RepairFlag,
  ) -> Result<StorePath> {
    let path = path.canonicalize()?;
    if ingest_method != FileIngestionMethod::Recursive {
      bail!("add a flat file")
    }

    let tmpdir = tempfile::tempdir_in(Path::new(&self.store_path()))?;
    let tmp_dest = tmpdir.as_ref().join("x");

    let nar_hash = Arc::new(Mutex::new(crate::hash::Sink::new(hash_type)));
    let nh = Arc::clone(&nar_hash);

    crossbeam::scope(|s| {
      let reader = make_pipe(s, move |writer| {
        crate::archive::dump_path(&path, TeeWriter::new(&mut *nh.lock(), writer), filter)
      });

      crate::archive::restore_path(&tmp_dest, reader)
    })
    .unwrap()?;

    let (nar_hash, nar_size) = nar_hash.lock().finish();

    let dest_path =
      self.make_fixed_output_path(ingest_method, &nar_hash, name, &mut iter::empty(), false)?;

    self.add_temp_root(&dest_path)?;

    if repair.repair() || !self.is_valid_path(&dest_path)? {
      let real_path = self.to_real_path(&dest_path)?;
      PathLocks::new().lock(iter::once(&real_path), false, None)?;
      if repair.repair() || !self.is_valid_path(&dest_path)? {
        delete_path(&real_path)?;
        fs::rename(tmp_dest, &real_path)?;

        canonicalise_path_metadata(&real_path, None)?;
        optimise_path(&real_path)?;

        let mut pi = ValidPathInfo::new(dest_path.clone(), nar_hash);
        pi.nar_size = Some(nar_size as _);
        self.register_valid_path(pi)?;
      }
    }

    debug!("deleting {}", tmpdir.as_ref().display());

    Ok(dest_path)
  }

  fn build_paths(&self, paths: Vec<StorePathWithOutputs>) -> Result<()> {
    let mut worker = Worker::with_store(self);
    for path in paths {
      worker.add_needed(&path.path)?;
    }
    worker.build()
  }

  fn compute_closure(
    &self,
    path: &StorePath,
    closure: &mut BTreeSet<StorePath>,
    options: ClosureOpts,
  ) -> Result<()> {
    if !closure.insert(path.clone()) {
      return Ok(());
    }

    let info = self
      .get_path_info(path)?
      .ok_or_else(|| anyhow!("path {} is invalid", self.print_store_path(path)))?;

    for r in info.references() {
      if r != path {
        self.compute_closure(r, closure, options)?;
      }
    }

    Ok(())
  }
}

impl LocalStore {
  pub fn open() -> Result<Self> {
    crate::globals::init()?;

    let settings = settings();

    let db_dir = settings.paths.nix_state_dir.join("db");

    fs::create_dir_all(&db_dir)?;

    let reserved_path = db_dir.join("reserved");
    if fs::metadata(&reserved_path)
      .map(|x| x.len() != settings.reserved_size)
      .unwrap_or(true)
    {
      let mut fd = File::create(&reserved_path)?;
      #[cfg(target_os = "linux")]
      unix::fcntl::posix_fallocate(fd.as_raw_fd(), 0, settings.reserved_size as _)?;
      if cfg!(not(target_os = "linux")) {
        fd.write_all(vec![b'X'; settings.reserved_size as usize].as_slice())?;
        ftruncate(fd.as_raw_fd(), settings.reserved_size as _)?;
      }
    }

    let schema_path = db_dir.join("schema");

    let global_lock_path = db_dir.join("big-lock");
    let lock = File::create(&global_lock_path).with_context(|| {
      format!(
        "while trying to open global lock file {}",
        global_lock_path.display()
      )
    })?;
    if !lock.try_lock(LockType::Write)? {
      info!("waiting for the big Nix store lock...");
      lock.lock(LockType::Write)?;
    }

    let cur_schema = match fs::read_to_string(&schema_path) {
      Ok(s) => Some(s.parse::<u32>()?),
      Err(_) => None,
    };

    let sqlite = Sqlite::open(&db_dir.join("db.sqlite"))?;

    if cur_schema.is_none() {
      db::init(&sqlite, true)?;
      fs::write(&schema_path, "10")?;
    } else {
      db::init(&sqlite, false)?;
    }

    let this = Self {
      temproots_dir: settings.paths.nix_state_dir.join("gcroots"),
      links_dir: settings.paths.nix_store.join(".links"),
      db: Mutex::new(sqlite),
    };
    fs::create_dir_all(&this.temproots_dir)?;
    fs::create_dir_all(&this.links_dir)?;
    Ok(this)
  }
}
