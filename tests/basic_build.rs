#![feature(map_first_last)]

#[macro_use] extern crate log;
use rix::{eval::Eval, path::PathWithOutputs, util::*};
use std::path::Path;

#[test]
fn test_basic_build() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let eval = Eval::new()?;

  let paths = vec![PathWithOutputs {
    path: eval.store.parse_store_path(Path::new(
      "/home/jude/.code/rust/rix/target/debug/build/rix-1152623a555af3cd/out/nix/store/\
       2khm01c4ryc52wi7gdbyjd93v7iz2y8b-hello-2.10.drv",
    ))?,
    outputs: std::iter::once("out".to_string()).collect(),
  }];

  info!("test: attempting to build out paths {:?}", paths);

  eval.store.build_paths(paths)?;

  Ok(())
}
