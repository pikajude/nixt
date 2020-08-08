use crate::prelude::*;
use std::{ffi::CStr, mem::MaybeUninit};
use unix::NixPath;

mod sys;

pub struct TarArchive {
  archive: *mut sys::archive,
}

impl TarArchive {
  pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
    let ptr = unsafe { sys::archive_read_new() };
    unsafe {
      sys::archive_read_support_filter_all(ptr);
      sys::archive_read_support_format_all(ptr);
    }
    let this = Self { archive: ptr };
    this.check(path.as_ref().with_nix_path(|cstr| unsafe {
      sys::archive_read_open_filename(this.archive, cstr.as_ptr(), 16384)
    })?)?;
    Ok(this)
  }

  fn check(&self, err: i32) -> Result<()> {
    if err == sys::ARCHIVE_EOF as i32 {
      bail!("reached end of archive")
    } else if err != sys::ARCHIVE_OK as i32 {
      let cstr = unsafe { CStr::from_ptr(sys::archive_error_string(self.archive)) };
      bail!("failed to extract archive: {}", cstr.to_str()?);
    }
    Ok(())
  }

  pub fn extract_to<P: AsRef<Path>>(self, dest: P) -> Result<()> {
    let dest = dest.as_ref();
    let flags = sys::ARCHIVE_EXTRACT_FFLAGS
      | sys::ARCHIVE_EXTRACT_PERM
      | sys::ARCHIVE_EXTRACT_TIME
      | sys::ARCHIVE_EXTRACT_SECURE_SYMLINKS
      | sys::ARCHIVE_EXTRACT_SECURE_NODOTDOT;

    unsafe {
      loop {
        let mut ent = MaybeUninit::<*mut sys::archive_entry>::uninit();
        let r = sys::archive_read_next_header(self.archive, ent.as_mut_ptr());
        if r == sys::ARCHIVE_EOF as i32 {
          break;
        } else if r == sys::ARCHIVE_WARN as i32 {
          let cstr = CStr::from_ptr(sys::archive_error_string(self.archive));
          warn!("{}", cstr.to_str()?);
        } else {
          self.check(r)?;
        }
        let ent = ent.assume_init();

        let pathname = sys::archive_entry_pathname(ent);
        let pathname_str = CStr::from_ptr(pathname);

        dest
          .join(pathname_str.to_str()?)
          .with_nix_path(|cstr| sys::archive_entry_set_pathname(ent, cstr.as_ptr()))?;

        self.check(sys::archive_read_extract(self.archive, ent, flags as i32))?;
      }
    }

    Ok(())
  }
}

impl Drop for TarArchive {
  fn drop(&mut self) {
    if let Err(e) = self.check(unsafe { sys::archive_read_close(self.archive) }) {
      info!("{}", e);
    }
    if !self.archive.is_null() {
      unsafe { sys::archive_read_free(self.archive) };
    }
  }
}
