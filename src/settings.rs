use once_cell::sync::Lazy;
use std::time::Duration;

static SETTINGS: Lazy<Settings> = Lazy::new(|| {
  eprintln!("parse from command line please");
  Settings::default()
});

#[derive(Debug, Clone)]
pub struct Settings {
  /// Open the store in read-only mode.
  pub read_only: bool,
  /// Seconds for which to consider files fetched with the builtin fetchers
  /// fresh.
  pub tarball_ttl: Duration,
  /// Amount of space reserved for the garbage collector.
  pub reserved_size: u64,
}

impl Settings {
  pub fn get() -> &'static Self {
    &*SETTINGS
  }
}

impl Default for Settings {
  fn default() -> Self {
    Self {
      read_only: false,
      tarball_ttl: Duration::from_secs(60 * 60),
      reserved_size: 8 * 1024 * 1024,
    }
  }
}
