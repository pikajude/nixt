use crate::prelude::*;
use libarchive::{archive::*, reader::*};
use libarchive3_sys::ffi;

pub struct Archive(FileReader);

impl Archive {
  pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
    let mut builder = Builder::new();
    builder.support_filter(ReadFilter::All)?;
    builder.support_format(ReadFormat::All)?;
    Ok(Self(builder.open_file(path)?))
  }

  pub fn extract_to<P: AsRef<Path>>(mut self, dest: P) -> Result<()> {
    let dest = dest.as_ref();
    let ptr = unsafe { self.0.handle() };
    let mut opts = ExtractOptions::new();
    opts
      .add(ExtractOption::FFlags)
      .add(ExtractOption::Permissions)
      .add(ExtractOption::Time)
      .add(ExtractOption::SecureSymlinks)
      .add(ExtractOption::SecureNoDotDot);

    while let Some(entry) = self.0.next_header() {
      let pathname = entry.pathname();
      let new_path = dest.join(pathname);
      entry.set_pathname(&new_path);
      let res = unsafe { ffi::archive_read_extract(ptr, entry.entry(), opts.flags) };
      if res != 0 {
        bail!("failed to extract archive: {}", self.0.err_msg())
      }
    }

    Ok(())
  }
}
