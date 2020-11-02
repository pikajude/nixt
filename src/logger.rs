use crate::prelude::Result;
use indicatif::ProgressBar;
use slog::{Discard, Drain, FnValue, OwnedKVList, Record, KV};
use slog_atomic::{AtomicSwitch, AtomicSwitchCtrl};
use std::sync::Mutex;

lazy_static! {
  // static ref PROGRESS_LOG: Mutex<Option<ProgressBar>> = Mutex::new(None);
  static ref DRAIN_SWITCH: AtomicSwitchCtrl<(), std::io::Error> =
    AtomicSwitch::new(Discard.map_err(|_| unreachable!())).ctrl();
}

struct ProgressLogger(ProgressBar);

struct ProgressFormatter<'a> {
  buf: &'a mut String,
}

impl<'a> slog::Serializer for ProgressFormatter<'a> {
  fn emit_arguments(&mut self, key: &'static str, val: &std::fmt::Arguments) -> slog::Result {
    self
      .buf
      .push_str(&format!(", \x1b[1m{}:\x1b[0m {}", key, val));
    Ok(())
  }
}

impl Drain for ProgressLogger {
  // actually Never, but this is more convenient
  type Err = std::io::Error;
  type Ok = ();

  fn log(&self, record: &Record, _: &OwnedKVList) -> std::result::Result<Self::Ok, Self::Err> {
    let mut msg = format!(
      "{} \x1b[3{}m{}\x1b[0m \x1b[1m{}\x1b[0m",
      record.module(),
      slog_term::TermDecorator::level_to_color(record.level()),
      record.level().as_short_str(),
      record.msg()
    );
    record
      .kv()
      .serialize(record, &mut ProgressFormatter { buf: &mut msg })?;
    self.0.println(msg.as_str());
    Ok(())
  }
}

pub fn set(progress: ProgressBar) {
  DRAIN_SWITCH.set(ProgressLogger(progress))
}

pub fn reset() {
  DRAIN_SWITCH.set(
    Mutex::new(slog_term::term_full())
      .map_err(|f| std::io::Error::new(std::io::ErrorKind::Other, f)),
  );
}

pub fn init() -> Result<()> {
  self::reset();

  let term_drain = DRAIN_SWITCH.drain();

  // #[cfg(debug_assertions)]
  // let file_drain = slog_json::Json::new(File::create("/tmp/rix.log")?)
  //   .add_key_value(slog::o!(
  //     "location" => FnValue(move |r: &Record| {
  //       format!("{}:{}", r.location().file, r.location().line)
  //     })
  //   ))
  //   .add_default_keys()
  //   .build();

  let file_drain = slog::Discard;

  let drain = std::sync::Mutex::new(slog_envlogger::new(
    slog::Duplicate::new(term_drain, file_drain).fuse(),
  ))
  .fuse();
  let log = slog::Logger::root(
    drain,
    slog::o!("location" => FnValue(move |r: &Record| {
      format!("{}:{}", r.location().file, r.location().line)
    })),
  );
  let guard = slog_scope::set_global_logger(log);
  std::mem::forget(guard);

  slog_stdlog::init()?;

  Ok(())
}
