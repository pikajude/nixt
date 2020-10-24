use indicatif::ProgressBar;
use log::{Level, Log, Metadata, Record};
use parking_lot::Mutex;
use pretty_env_logger::env_logger;
use std::{
  fmt,
  io::Write,
  sync::atomic::{AtomicUsize, Ordering},
};
use termcolor::{Color, ColorSpec, WriteColor};

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
          let target = record.target();
          let max_width = max_target_width(target);

          let target = Padded {
            value: target,
            width: max_width,
          };

          let mut buf = termcolor::Buffer::ansi();

          buf.write_all(b" ").unwrap();

          buf
            .set_color(&ColorSpec::new().set_fg(Some(match record.level() {
              Level::Trace => Color::Magenta,
              Level::Debug => Color::Blue,
              Level::Info => Color::Green,
              Level::Warn => Color::Yellow,
              Level::Error => Color::Red,
            })))
            .unwrap();
          buf
            .write_all(match record.level() {
              Level::Trace => b"TRACE",
              Level::Debug => b"DEBUG",
              Level::Info => b"INFO ",
              Level::Warn => b"WARN ",
              Level::Error => b"ERROR",
            })
            .unwrap();
          buf.write_all(b" ").unwrap();
          buf
            .set_color(&ColorSpec::new().set_fg(None).set_bold(true))
            .unwrap();
          write!(buf, "{}", target).unwrap();
          buf.set_color(&ColorSpec::new().set_bold(false)).unwrap();
          write!(buf, " > {}", record.args()).unwrap();

          prog.println(unsafe { std::str::from_utf8_unchecked(buf.as_slice()) });
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
struct Padded<T> {
  value: T,
  width: usize,
}

impl<T: fmt::Display> fmt::Display for Padded<T> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{: <width$}", self.value, width = self.width)
  }
}

static MAX_MODULE_WIDTH: AtomicUsize = AtomicUsize::new(0);

fn max_target_width(target: &str) -> usize {
  let max_width = MAX_MODULE_WIDTH.load(Ordering::Relaxed);
  if max_width < target.len() {
    MAX_MODULE_WIDTH.store(target.len(), Ordering::Relaxed);
    target.len()
  } else {
    max_width
  }
}
