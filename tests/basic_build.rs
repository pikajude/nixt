#![feature(map_first_last)]

#[macro_use] extern crate log;
use rix::{eval::Eval, path::PathWithOutputs, util::*};
use std::path::Path;

#[test]
fn test_basic_build() -> Result<()> {
  pretty_env_logger::init();

  let eval = Eval::new()?;

  let paths = vec![PathWithOutputs {
    path: eval.store.parse_store_path(Path::new(
      "/nix/store/zknkvnmxgl4yz8013h439qlv4ay9qmvs-hello-2.10.drv",
    ))?,
    outputs: std::iter::once("out".to_string()).collect(),
  }];

  info!("test: attempting to build out paths {:?}", paths);

  eval.store.build_paths(paths)?;

  Ok(())
}
