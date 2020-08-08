use super::{CheckSigsFlag, FileIngestionMethod, RepairFlag};
use crate::{
  archive::{self, PathFilter},
  hash::{Encoding, Hash, HashType},
  path::Path as StorePath,
  path_info::{PathInfo, ValidPathInfo},
  settings,
  sqlite::Sqlite,
  util::*,
  Store,
};
use fs::File;
use lock::{FsExt2, LockType};
#[cfg(unix)] use std::os::unix::io::AsRawFd;
use std::{
  borrow::Cow,
  collections::HashSet,
  ffi::OsStr,
  fs,
  io::Write,
  iter,
  os::unix::fs::*,
  path::{Path, PathBuf},
  rc::Rc,
  sync::Mutex,
};
use tee_readwrite::TeeWriter;
use unix::{
  libc,
  sys::{stat::*, time::TimeSpec},
  unistd::*,
};

mod db;
mod gc;
mod lock;

#[derive(Eq, PartialEq, Hash)]
struct Inode {
  dev: u64,
  ino: u64,
}

#[derive(Default)]
pub struct OptimiseStats {
  pub files_linked: u32,
  pub bytes_freed: u64,
  pub blocks_freed: u64,
}

pub struct LocalStore {
  store_dir: PathBuf,
  state_dir: PathBuf,
  temproots_dir: PathBuf,
  links_dir: PathBuf,
  db_dir: PathBuf,
  db: Mutex<Sqlite>,
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
    repair: RepairFlag,
  ) -> Result<StorePath> {
    let repair = repair == RepairFlag::Repair;
    let hash = Hash::hash_str(contents, HashType::SHA256);
    let dest_path = self.make_text_path(name, &hash, references)?;

    self.add_temp_root(&dest_path)?;

    if repair || !self.is_valid_path(&dest_path)? {
      let real_path = self.to_real_path(&dest_path)?;

      let _ = lock::PathLocks::new().lock(iter::once(&real_path), true, None)?;

      if repair || !self.is_valid_path(&dest_path)? {
        self.delete_path(&real_path)?;

        fs::write(&real_path, contents)?;

        debug!("added {} to store", real_path.display());

        self.canonicalise_path_metadata(&real_path, None)?;

        let nar_bytes = archive::dump_to_bytes(contents.len(), contents.as_bytes())?;
        let nar_hash = Hash::hash_bytes(&nar_bytes, HashType::SHA256);

        self.optimise_path(&real_path)?;

        let mut path_info = ValidPathInfo::new(dest_path.clone(), nar_hash);
        path_info.nar_size = Some(nar_bytes.len() as u64);
        path_info.references = references.cloned().collect();

        self.register_valid_path(path_info)?;
      }
    }

    Ok(dest_path)
  }

  fn get_path_info(&self, path: &StorePath) -> Result<Option<Rc<dyn PathInfo>>> {
    let conn = self.db.lock().unwrap();
    if let Some(x) = db::get_path_info(&conn, self, path)? {
      Ok(Some(Rc::new(x)))
    } else {
      Ok(None)
    }
  }

  fn add_temp_root(&self, path: &StorePath) -> Result<()> {
    let file = self.temproots_dir.join(std::process::id().to_string());
    let mut temp_file = loop {
      let all_gc_roots = gc::open_gc_lock(&self.state_dir, LockType::Read)?;
      self.delete_path(&file)?;
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

  fn register_valid_path(&self, _path_info: ValidPathInfo) -> Result<()> {
    let mut sql = self.db.lock().unwrap();
    db::insert_valid_paths(&mut sql, self, iter::once(&_path_info))?;
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

      lock::PathLocks::new().lock(&mut iter::once(&dest), true, None)?;

      if repair.repair() || !self.is_valid_path(path_info.store_path())? {
        self.delete_path(&dest)?;

        archive::restore_path(&dest, input)?;

        self.canonicalise_path_metadata(&dest, None)?;
      }
    }

    Ok(())
  }

  fn add_to_store_from_path(
    &self,
    name: &str,
    path: &Path,
    ingest_method: FileIngestionMethod,
    _hash_type: HashType,
    filter: &PathFilter,
    repair: RepairFlag,
  ) -> Result<StorePath> {
    let path = path.canonicalize()?;
    if ingest_method != FileIngestionMethod::Recursive {
      bail!("add a flat file")
    }

    let tmpdir = tempfile::tempdir_in(Path::new(&self.store_path()))?.into_path();

    let mut nar_hash = crate::hash::Sink::new(HashType::SHA256);

    crate::archive::restore_path(
      &tmpdir,
      SourceSink::new(|sink| {
        crate::archive::dump_path(&path, TeeWriter::new(&mut nar_hash, sink), filter)
      }),
    )?;

    let (nar_hash, nar_size) = nar_hash.finish();

    let dest_path =
      self.make_fixed_output_path(ingest_method, &nar_hash, name, &mut iter::empty(), false)?;

    self.add_temp_root(&dest_path)?;

    if repair.repair() || !self.is_valid_path(&dest_path)? {
      let real_path = self.to_real_path(&dest_path)?;
      lock::PathLocks::new().lock(iter::once(&real_path), false, None)?;
      if repair.repair() || !self.is_valid_path(&dest_path)? {
        self.delete_path(&real_path)?;
        std::fs::rename(&tmpdir, &real_path)?;

        self.canonicalise_path_metadata(&real_path, None)?;
        self.optimise_path(&real_path)?;

        let mut pi = ValidPathInfo::new(dest_path.clone(), nar_hash);
        pi.nar_size = Some(nar_size as _);
        self.register_valid_path(pi)?;
      }
    }

    Ok(dest_path)
  }
}

impl LocalStore {
  pub fn open() -> Result<Self> {
    #[cfg(test)]
    let _ = pretty_env_logger::try_init();

    let root = concat!(env!("OUT_DIR"), "/nix");
    let root_dir = PathBuf::from(root);
    let db_dir = root_dir.join("var").join("nix").join("db");

    fs::create_dir_all(&db_dir)?;

    let reserved_path = db_dir.join("reserved");
    if fs::metadata(&reserved_path)
      .map(|x| x.len() != settings().reserved_size)
      .unwrap_or(true)
    {
      #[allow(unused_mut)]
      let mut fd = File::create(&reserved_path)?;
      #[cfg(target_os = "linux")]
      unix::fcntl::posix_fallocate(fd.as_raw_fd(), 0, settings().reserved_size as _)?;
      #[cfg(not(target_os = "linux"))]
      {
        fd.write_all(vec![b'X'; settings().reserved_size as usize].as_slice())?;
        ftruncate(fd.as_raw_fd(), settings().reserved_size as _)?;
      }
    }

    let schema_path = db_dir.join("schema");

    let global_lock_path = db_dir.join("big-lock");
    let lock = File::create(&global_lock_path)?;
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
      store_dir: root_dir.join("store"),
      state_dir: root_dir.join("var").join("nix"),
      temproots_dir: root_dir.join("var").join("nix").join("gcroots"),
      links_dir: root_dir.join("store").join(".links"),
      db_dir: root_dir.join("var").join("nix").join("db"),
      db: Mutex::new(sqlite),
    };
    fs::create_dir_all(&this.store_dir)?;
    fs::create_dir_all(&this.temproots_dir)?;
    fs::create_dir_all(&this.links_dir)?;
    Ok(this)
  }

  fn delete_path(&self, p: &Path) -> Result<u64> {
    if !p.exists() {
      return Ok(0);
    }
    let mut bytes_freed = 0;
    self.delete_path_impl(p, &mut bytes_freed)?;
    Ok(bytes_freed)
  }

  fn delete_path_impl(&self, path: &Path, bytes_freed: &mut u64) -> Result<()> {
    let meta = fs::symlink_metadata(path)?;
    if meta.is_file() && meta.nlink() == 1 {
      *bytes_freed += meta.len();
    } else if meta.is_dir() {
      let cur_mode = Mode::from_bits_truncate(meta.mode() as _);
      let target_mode = Mode::S_IRUSR | Mode::S_IWUSR | Mode::S_IXUSR;
      if !cur_mode.contains(target_mode) {
        let mut perms = meta.permissions();
        perms.set_mode((cur_mode | target_mode).bits() as _);
        fs::set_permissions(path, perms)
          .with_context(|| format!("while making `{}' writable", path.display()))?;
      }
      for file in fs::read_dir(path)? {
        self.delete_path_impl(&file?.path(), bytes_freed)?;
      }

      fs::remove_dir(path)?;
      return Ok(());
    }
    fs::remove_file(path)
      .with_context(|| format!("while trying to delete file `{}'", path.display()))?;
    Ok(())
  }

  fn canonicalise_path_metadata<P: AsRef<Path>>(
    &self,
    path: P,
    uid: Option<libc::uid_t>,
  ) -> Result<()> {
    self.canonicalise_path_metadata_impl(path, uid, &mut HashSet::new())
  }

  fn canonicalise_path_metadata_impl<P: AsRef<Path>>(
    &self,
    path: P,
    uid: Option<libc::uid_t>,
    inodes: &mut HashSet<Inode>,
  ) -> Result<()> {
    use std::ffi::CString;
    use unix::errno::*;

    let path = path.as_ref();
    let path_str = CString::new(path.to_string_lossy().into_owned())?;

    #[cfg(target_os = "macos")]
    {
      extern "C" {
        fn lchflags(path: *const libc::c_char, flags: libc::c_ulong) -> libc::c_int;
      }

      if unsafe { lchflags(path_str.as_ptr(), 0) } != 0 {
        if errno() != libc::ENOTSUP {
          bail!(std::io::Error::last_os_error());
        }
      }
    }

    #[cfg(target_os = "linux")]
    {
      extern "C" {
        fn llistxattr(
          path: *const libc::c_char,
          list: *mut libc::c_char,
          size: libc::size_t,
        ) -> libc::ssize_t;
        fn lremovexattr(path: *const libc::c_char, name: *const libc::c_char) -> libc::c_int;
      }

      use std::cmp::Ordering;

      let attrsize = unsafe { llistxattr(path_str.as_ptr(), std::ptr::null_mut(), 0) };
      match attrsize.cmp(&0) {
        Ordering::Less => {
          if errno() != libc::ENOTSUP && errno() != libc::ENODATA {
            bail!("querying extended attributes of `{}'", path.display());
          }
        }
        Ordering::Greater => {
          let mut attrbuf = vec![0u8; attrsize as usize];
          unsafe {
            llistxattr(
              path_str.as_ptr(),
              attrbuf.as_mut_ptr() as *mut _,
              attrbuf.len(),
            )
          };
          for attr_name in attrbuf.split(|x| *x == 0) {
            if attr_name == b"security.selinux" {
              continue;
            }
            if unsafe { lremovexattr(path_str.as_ptr(), attr_name.as_ptr() as *const _) } == -1 {
              bail!(
                "removing extended attribute `{}' from `{}'",
                String::from_utf8_lossy(attr_name),
                path.display()
              );
            }
          }
        }
        _ => {}
      }
    }

    let info = std::fs::symlink_metadata(path)?;
    let ty = info.file_type();

    if !ty.is_file() && !ty.is_dir() && !ty.is_symlink() {
      bail!("file `{}' has an unsupported type", path.display());
    }

    if uid.map_or(false, |x| x != info.uid()) {
      assert!(!ty.is_dir());
      if !inodes.contains(&Inode {
        dev: info.dev(),
        ino: info.ino(),
      }) {
        bail!("invalid ownership on file `{}'", path.display());
      }

      let mode = info.mode() & !(libc::S_IFMT as u32);
      assert!(
        ty.is_symlink()
          || (info.uid() == getuid().as_raw()
            && (mode == 0o444 || mode == 0o555)
            && info.mtime() == 1)
      );
    }

    inodes.insert(Inode {
      dev: info.dev(),
      ino: info.ino(),
    });

    self.canonicalize_timestamp_and_permissions(path, &info)?;

    if info.uid() != geteuid().as_raw() {
      fchownat(
        None,
        path,
        Some(geteuid()),
        Some(getegid()),
        FchownatFlags::NoFollowSymlink,
      )
      .with_context(|| format!("while changing ownership of path `{}'", path.display()))?;
    }

    if ty.is_dir() {
      for entry in fs::read_dir(path)? {
        self.canonicalise_path_metadata_impl(entry?.path(), uid, inodes)?;
      }
    }

    Ok(())
  }

  fn canonicalize_timestamp_and_permissions<P: AsRef<Path>>(
    &self,
    path: P,
    info: &fs::Metadata,
  ) -> Result<()> {
    if !info.file_type().is_symlink() {
      let mut mode = info.mode() & !(libc::S_IFMT as u32);
      if mode != 0o444 && mode != 0o555 {
        mode = (info.mode() & libc::S_IFMT as u32)
          | 0o444
          | (if info.mode() & libc::S_IXUSR as u32 > 0 {
            0o111
          } else {
            0
          });
        fchmodat(
          None,
          path.as_ref(),
          Mode::from_bits_truncate(mode as libc::mode_t),
          FchmodatFlags::FollowSymlink,
        )?;
      }
    }

    if info.mtime() != 1 {
      use unix::sys::time::TimeValLike;
      utimensat(
        None,
        path.as_ref(),
        &TimeSpec::seconds(info.atime()),
        &TimeSpec::seconds(1),
        UtimensatFlags::NoFollowSymlink,
      )?;
    }

    Ok(())
  }

  fn optimise_path(&self, path: &Path) -> Result<()> {
    let mut stats = OptimiseStats::default();
    self.optimise_path_impl(path, &mut HashSet::new(), &mut stats)?;
    info!(
      "{} files hard-linked, {} bytes freed",
      stats.files_linked, stats.bytes_freed,
    );
    Ok(())
  }

  fn optimise_path_impl(
    &self,
    path: &Path,
    inodes: &mut HashSet<libc::ino_t>,
    stats: &mut OptimiseStats,
  ) -> Result<()> {
    #[cfg(target_os = "macos")]
    {
      if path.to_string_lossy().contains(".app/Contents/") {
        debug!("path `{}' cannot be linked", path.display());
        return Ok(());
      }
    }

    let info = fs::symlink_metadata(path)?;
    let ty = info.file_type();

    if ty.is_dir() {
      for x in fs::read_dir(path)? {
        let x = x?;
        let this_path = x.path();
        let meta = x.metadata()?;
        if inodes.contains(&meta.ino()) {
          debug!("path `{}' is already linked", this_path.display());
          continue;
        }
        self.optimise_path_impl(&this_path, inodes, stats)?;
      }
      return Ok(());
    }

    if !ty.is_file() && !ty.is_symlink() {
      return Ok(());
    }

    if ty.is_file() && info.mode() & libc::S_IWUSR as u32 != 0 {
      warn!("ignoring suspicious writable file `{}'", path.display());
      return Ok(());
    }

    if info.nlink() > 1 && inodes.contains(&info.ino()) {
      debug!(
        "`{}' is already linked to {} other file(s)",
        path.display(),
        info.nlink() - 2
      );
      return Ok(());
    }

    let (file_hash, _) = archive::hash_path(path, HashType::SHA256, &PathFilter::none())?;

    debug!(
      "path `{}' has hash `{}'",
      path.display(),
      file_hash.encode_with_type(Encoding::Base32)
    );

    let link_path = self.links_dir.join(file_hash.encode(Encoding::Base32));

    loop {
      if fs::metadata(&link_path).is_err() {
        match fs::hard_link(path, &link_path) {
          Ok(_) => {
            inodes.insert(info.ino());
            return Ok(());
          }
          Err(e) => {
            if e.raw_os_error() == Some(libc::ENOSPC) {
              info!(
                "cannot link `{}' to `{}': {}",
                link_path.display(),
                path.display(),
                e
              );
              return Ok(());
            } else if e.raw_os_error() != Some(libc::EEXIST) {
              bail!(
                "cannot link `{}' to `{}': {}",
                link_path.display(),
                path.display(),
                e
              );
            }
          }
        }
      }

      let info_link = fs::symlink_metadata(&link_path)?;
      if info.ino() == info_link.ino() {
        debug!(
          "`{}' is already linked to `{}'",
          path.display(),
          link_path.display()
        );
        return Ok(());
      }
      if info.size() != info_link.size() {
        warn!("removing corrupted link `{}'", link_path.display());
        self.delete_path(&link_path)?;
        continue;
      }

      info!("linking `{}' to `{}'", path.display(), link_path.display());

      let parent = path.parent().unwrap();
      let need_temp_permissions = parent != self.store_dir;

      if need_temp_permissions {
        let meta = fs::metadata(parent)?;
        let mut perms = meta.permissions();
        perms.set_mode(perms.mode() | 0o700);
        fs::set_permissions(parent, perms)?;
      }

      let temp_link = self.store_dir.join(format!(
        ".tmp-link-{}-{}",
        std::process::id(),
        rand::random::<u32>()
      ));

      fs::hard_link(&link_path, &temp_link)?;
      fs::rename(&temp_link, path).with_context(|| {
        format!(
          "while trying to move `{}' to `{}'",
          temp_link.display(),
          path.display()
        )
      })?;

      stats.files_linked += 1;
      stats.bytes_freed += info.size();
      stats.blocks_freed += info.blocks();

      if need_temp_permissions {
        self.canonicalize_timestamp_and_permissions(parent, &fs::symlink_metadata(parent)?)?;
      }

      break;
    }

    Ok(())
  }
}
