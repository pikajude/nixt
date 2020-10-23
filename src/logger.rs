use indicatif::ProgressBar;
use log::{Log, Metadata, Record};
use parking_lot::Mutex;
use pretty_env_logger::env_logger;

lazy_static! {
  static ref PROGRESS_LOG: Mutex<Option<ProgressBar>> = Mutex::new(None);
}

pub struct Logger {
  inner: env_logger::Logger,
}

impl Log for Logger {
  fn enabled(&self, metadata: &Metadata) -> bool {
    self.inner.enabled(metadata)
  }

  fn log(&self, record: &Record) {
    PROGRESS_LOG.lock().as_ref().map_or_else(
      || self.inner.log(record),
      |prog| {
        if self.inner.matches(record) {
          prog.println(format!(
            "{} [{}] {}",
            record.level(),
            record.target(),
            record.args()
          ));
        }
      },
    );
  }

  fn flush(&self) {
    if PROGRESS_LOG.lock().is_none() {
      self.inner.flush()
    }
  }
}

impl Logger {
  pub fn set(l: ProgressBar) {
    *PROGRESS_LOG.lock() = Some(l)
  }

  pub fn reset() {
    PROGRESS_LOG.lock().take().unwrap();
  }

  pub fn init() {
    let mut builder = pretty_env_logger::formatted_builder();
    if let Ok(s) = std::env::var("RUST_LOG") {
      builder.parse_filters(&s);
    }
    let inner = builder.build();
    let level = inner.filter();
    log::set_boxed_logger(Box::new(Logger { inner })).unwrap();
    log::set_max_level(level);
  }
}
