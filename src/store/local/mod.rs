use crate::{
  archive::{self, PathFilter},
  hash::{Encoding, Hash, HashType},
  path::Path as StorePath,
  path_info::{PathInfo, ValidPathInfo},
  util::*,
  Store,
};
use lock::{FsExt2, LockType};
#[cfg(target_os = "linux")] use std::os::linux::fs::MetadataExt as _;
#[cfg(target_os = "macos")] use std::os::macos::fs::MetadataExt as _;
use std::{
  borrow::Cow,
  collections::HashSet,
  ffi::OsStr,
  fs,
  io::Write,
  os::unix::fs::*,
  path::{Path, PathBuf},
  sync::Arc,
};
use unix::{
  libc::{self, mode_t},
  sys::{stat::*, time::TimeSpec},
  unistd::*,
};

mod gc;
mod lock;

#[derive(Eq, PartialEq, Hash)]
struct Inode {
  dev: libc::dev_t,
  ino: libc::ino_t,
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

    if repair || !self.is_valid_path(&dest_path)? {
      let real_path = self.to_real_path(&dest_path)?;

      let _ = lock::PathLocks::new().lock(std::iter::once(&real_path), true, None)?;

      if repair || !self.is_valid_path(&dest_path)? {
        self.delete_path(&real_path)?;

        fs::write(&real_path, contents)?;

        debug!("added {} to store", real_path.display());

        self.canonicalise_path_metadata(&real_path, None)?;

        let nar_bytes = archive::dump_to_bytes(contents)?;
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

  fn get_path_info(&self, path: &StorePath) -> Result<Option<Arc<dyn PathInfo>>> {
    debug!("path info: {}", path);
    Ok(None)
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

  fn register_valid_path(&self, _path_info: ValidPathInfo) -> Result<()> {
    todo!()
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
      links_dir: root_dir.join("store").join(".links"),
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
    let meta = fs::metadata(path)?;
    if meta.is_file() && meta.st_nlink() == 1 {
      *bytes_freed += meta.len();
    } else if meta.is_dir() {
      let cur_mode = Mode::from_bits_truncate(meta.mode() as _);
      let target_mode = Mode::S_IRUSR | Mode::S_IWUSR | Mode::S_IXUSR;
      if !cur_mode.contains(target_mode) {
        let mut perms = meta.permissions();
        perms.set_mode((cur_mode & target_mode).bits() as _);
        fs::set_permissions(path, perms)
          .with_context(|| format!("while making `{}' writable", path.display()))?;
      }
      for file in fs::read_dir(path)? {
        self.delete_path_impl(&file?.path(), bytes_freed)?;
      }

      fs::remove_dir(path)?;
      return Ok(());
    }

    fs::remove_file(path)?;
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

    let info = lstat(path)?;
    if !(s_isreg(info.st_mode) || s_isdir(info.st_mode) || s_islnk(info.st_mode)) {
      bail!("file `{}' has an unsupported type", path.display());
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

    if uid.map_or(false, |x| x != info.st_uid) {
      assert!(!s_isdir(info.st_mode));
      if !inodes.contains(&Inode {
        dev: info.st_dev,
        ino: info.st_ino,
      }) {
        bail!("invalid ownership on file `{}'", path.display());
      }

      let mode = info.st_mode & !libc::S_IFMT;
      assert!(
        s_islnk(mode)
          || (info.st_uid == getuid().as_raw()
            && (mode == 0o444 || mode == 0o555)
            && info.st_mtime == 1)
      );
    }

    inodes.insert(Inode {
      dev: info.st_dev,
      ino: info.st_ino,
    });

    self.canonicalize_timestamp_and_permissions(path, info)?;

    if info.st_uid != geteuid().as_raw() {
      fchownat(
        None,
        path,
        Some(geteuid()),
        Some(getegid()),
        FchownatFlags::NoFollowSymlink,
      )
      .with_context(|| format!("while changing ownership of path `{}'", path.display()))?;
    }

    if s_isdir(info.st_mode) {
      for entry in fs::read_dir(path)? {
        self.canonicalise_path_metadata_impl(entry?.path(), uid, inodes)?;
      }
    }

    Ok(())
  }

  fn canonicalize_timestamp_and_permissions<P: AsRef<Path>>(
    &self,
    path: P,
    info: FileStat,
  ) -> Result<()> {
    if !s_islnk(info.st_mode) {
      let mut mode = info.st_mode & !libc::S_IFMT;
      if mode != 0o444 && mode != 0o555 {
        mode = (info.st_mode & libc::S_IFMT)
          | 0o444
          | (if info.st_mode & libc::S_IXUSR > 0 {
            0o111
          } else {
            0
          });
        fchmodat(
          None,
          path.as_ref(),
          Mode::from_bits_truncate(mode),
          FchmodatFlags::FollowSymlink,
        )?;
      }
    }

    if info.st_mtime != 1 {
      use unix::sys::time::TimeValLike;
      utimensat(
        None,
        path.as_ref(),
        &TimeSpec::seconds(info.st_atime),
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
    let stat = lstat(path)?;

    #[cfg(target_os = "macos")]
    {
      if path.to_string_lossy().contains(".app/Contents/") {
        debug!("path `{}' cannot be linked", path.display());
        return Ok(());
      }
    }

    if s_isdir(stat.st_mode) {
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

    if !s_isreg(stat.st_mode) && !s_islnk(stat.st_mode) {
      return Ok(());
    }

    if s_isreg(stat.st_mode) && (stat.st_mode & libc::S_IWUSR != 0) {
      warn!("ignoring suspicious writable file `{}'", path.display());
      return Ok(());
    }

    if stat.st_nlink > 1 && inodes.contains(&stat.st_ino) {
      debug!(
        "`{}' is already linked to {} other file(s)",
        path.display(),
        stat.st_nlink - 2
      );
      return Ok(());
    }

    let (file_hash, _) = archive::hash_path(path, HashType::SHA256, &PathFilter::no_filter())?;

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
            inodes.insert(stat.st_ino);
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

      let st_link = lstat(&link_path)?;
      if stat.st_ino == st_link.st_ino {
        debug!(
          "`{}' is already linked to `{}'",
          path.display(),
          link_path.display()
        );
        return Ok(());
      }
      if stat.st_size != st_link.st_size {
        warn!("removing corrupted link `{}'", link_path.display());
        let _ = fs::remove_file(&link_path);
        continue;
      }

      info!("linking `{}' to `{}'", path.display(), link_path.display());

      if path.parent() != Some(&self.store_dir) {
        // make_writable(path.parent())
      }

      let temp_link = self.store_dir.join(format!(
        ".tmp-link-{}-{}",
        std::process::id(),
        rand::random::<u32>()
      ));

      fs::hard_link(&link_path, &temp_link)?;
      fs::rename(&temp_link, path)?;

      stats.files_linked += 1;
      stats.bytes_freed += stat.st_size as u64;
      stats.blocks_freed += stat.st_blocks as u64;

      break;
    }

    Ok(())
  }
}

fn s_isreg(mode: mode_t) -> bool {
  (mode & SFlag::S_IFMT.bits()) == SFlag::S_IFREG.bits()
}
fn s_isdir(mode: mode_t) -> bool {
  (mode & SFlag::S_IFMT.bits()) == SFlag::S_IFDIR.bits()
}
fn s_islnk(mode: mode_t) -> bool {
  (mode & SFlag::S_IFMT.bits()) == SFlag::S_IFLNK.bits()
}
