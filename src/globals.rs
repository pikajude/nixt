use crate::prelude::*;
use std::env;

pub fn init() -> Result<()> {
  let _ = settings();

  // ctrlc::set_handler(move || {
  //   println!("received ctrl-c");
  // })?;

  if cfg!(target_os = "macos")
    && env::var("TMPDIR").map_or(false, |x| x.starts_with("/var/folders"))
  {
    env::remove_var("TMPDIR");
  }

  crate::logger::Logger::init();

  Ok(())
}
