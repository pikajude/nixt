use crate::{archive, prelude::*, store::OptimiseStats};
use archive::PathFilter;
use std::{collections::HashSet, fs, os::unix::fs::PermissionsExt};
use unix::{
  errno::Errno,
  sys::{stat::*, time::TimeVal},
  unistd::*,
};

#[derive(Eq, PartialEq, Debug, Hash)]
pub struct Inode {
  pub dev: u64,
  pub ino: u64,
}

pub fn rm_rf<P: AsRef<Path>>(path: P) -> Result<()> {
  let path = path.as_ref();

  let meta = match std::fs::symlink_metadata(path) {
    Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(()),
    x => x?,
  };

  if !meta.file_type().is_symlink() {
    let mut perm = meta.permissions();
    let mode = perm.mode();
    perm.set_mode(mode | 0o200);
    std::fs::set_permissions(path, perm).with_context(|| "setting permissions")?;
  }

  if meta.is_dir() {
    for entry in std::fs::read_dir(path)? {
      rm_rf(entry?.path())?;
    }
    std::fs::remove_dir(path).with_context(|| "removing directory")
  } else {
    std::fs::remove_file(path).with_context(|| "removing file")
  }
}

pub fn canonicalise_path_metadata<P: AsRef<Path>>(path: P, uid: Option<u32>) -> Result<()> {
  canonicalise_path_metadata_impl(path, uid, &mut HashSet::new())
}

fn canonicalise_path_metadata_impl<P: AsRef<Path>>(
  path: P,
  uid: Option<u32>,
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
      if Errno::last() != Errno::ENOTSUP {
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
        if Errno::last() != ENOTSUP && Errno::last() != Errno::ENODATA {
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

  let info = fs::symlink_metadata(path)?;
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

    let mode = info.mode() & !(SFlag::S_IFMT.bits() as u32);
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

  canonicalize_timestamp_and_permissions(path, &info)?;

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
      canonicalise_path_metadata_impl(
        entry
          .with_context(|| format!("while reading dir entries in {}", path.display()))?
          .path(),
        uid,
        inodes,
      )?;
    }
  }

  Ok(())
}

pub fn canonicalize_timestamp_and_permissions<P: AsRef<Path>>(
  path: P,
  info: &fs::Metadata,
) -> Result<()> {
  if !info.file_type().is_symlink() {
    let mut mode = info.mode() & !(SFlag::S_IFMT.bits() as u32);
    if mode != 0o444 && mode != 0o555 {
      mode = (info.mode() & (SFlag::S_IFMT.bits() as u32))
        | 0o444
        | (if info.mode() & (Mode::S_IXUSR.bits() as u32) > 0 {
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
    lutimes(
      path.as_ref(),
      &TimeVal::seconds(info.atime()),
      &TimeVal::seconds(1),
    )?;
  }

  Ok(())
}

pub fn optimise_path(path: &Path) -> Result<()> {
  let mut stats = OptimiseStats::default();
  optimise_path_impl(path, &mut HashSet::new(), &mut stats)?;
  info!(
    "{} files hard-linked, {} bytes freed",
    stats.files_linked, stats.bytes_freed,
  );
  Ok(())
}

fn optimise_path_impl(
  path: &Path,
  inodes: &mut HashSet<u64>,
  stats: &mut OptimiseStats,
) -> Result<()> {
  if cfg!(target_os = "macos") && path.to_string_lossy().contains(".app/Contents/") {
    debug!("path `{}' cannot be linked", path.display());
    return Ok(());
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
      optimise_path_impl(&this_path, inodes, stats)?;
    }
    return Ok(());
  }

  if !ty.is_file() && !ty.is_symlink() {
    return Ok(());
  }

  if ty.is_file() && info.mode() & (Mode::S_IWUSR.bits() as u32) != 0 {
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

  let link_path = settings()
    .paths
    .nix_store
    .join(".links")
    .join(file_hash.encode(Encoding::Base32));

  loop {
    if fs::metadata(&link_path).is_err() {
      match fs::hard_link(path, &link_path) {
        Ok(_) => {
          inodes.insert(info.ino());
          return Ok(());
        }
        Err(e) => {
          let errno = e.raw_os_error().map(Errno::from_i32);
          if errno == Some(Errno::ENOSPC) {
            info!(
              "cannot link `{}' to `{}': {}",
              link_path.display(),
              path.display(),
              e
            );
            return Ok(());
          } else if errno != Some(Errno::EEXIST) {
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
      delete_path(&link_path)?;
      continue;
    }

    info!("linking `{}' to `{}'", path.display(), link_path.display());

    let parent = path.parent().unwrap();
    let need_temp_permissions = parent != settings().paths.nix_store;

    if need_temp_permissions {
      let meta = fs::metadata(parent)?;
      let mut perms = meta.permissions();
      perms.set_mode(perms.mode() | 0o700);
      fs::set_permissions(parent, perms)?;
    }

    let temp_link = settings().paths.nix_store.join(format!(
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
      canonicalize_timestamp_and_permissions(parent, &fs::symlink_metadata(parent)?)?;
    }

    break;
  }

  Ok(())
}

pub fn delete_path(p: &Path) -> Result<u64> {
  if !p.exists() {
    return Ok(0);
  }
  let mut bytes_freed = 0;
  delete_path_impl(p, &mut bytes_freed)?;
  Ok(bytes_freed)
}

fn delete_path_impl(path: &Path, bytes_freed: &mut u64) -> Result<()> {
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
      delete_path_impl(&file?.path(), bytes_freed)?;
    }

    fs::remove_dir(path)?;
    return Ok(());
  }
  fs::remove_file(path)
    .with_context(|| format!("while trying to delete file `{}'", path.display()))?;
  Ok(())
}
