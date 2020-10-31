use crate::prelude::Result;
use indicatif::ProgressBar;
use parking_lot::Mutex;
use slog::{Drain, FnValue, Record};
use std::fs::File;

lazy_static! {
  static ref PROGRESS_LOG: Mutex<Option<ProgressBar>> = Mutex::new(None);
}

pub struct Logger;

impl Logger {
  pub fn set(_: ProgressBar) {}

  pub fn reset() {}

  pub fn init() -> Result<()> {
    let decorator = slog_term::TermDecorator::new().build();
    let term_drain = slog_term::FullFormat::new(decorator).build();

    #[cfg(debug_assertions)]
    let file_drain = slog_json::Json::new(File::create("/tmp/rix.log")?)
      .add_key_value(slog::o!(
        "location" => FnValue(move |r: &Record| {
          format!("{}:{}", r.location().file, r.location().line)
        })
      ))
      .add_default_keys()
      .build();

    #[cfg(not_debug_assertions)]
    let file_drain = slog::Discard;

    let drain = slog_envlogger::new(
      file_drain.fuse(), /* slog::Duplicate::new(term_drain, file_drain).fuse() */
    );
    let drain = slog_async::Async::new(drain).build().fuse();
    let log = slog::Logger::root(drain, slog::o!());
    let guard = slog_scope::set_global_logger(log);
    std::mem::forget(guard);

    slog_stdlog::init()?;

    Ok(())
  }
}
