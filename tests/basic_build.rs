#![feature(map_first_last)]

#[macro_use] extern crate log;
use rnix::{eval::Eval, path::PathWithOutputs, util::*};
use std::path::Path;

#[test]
fn test_basic_build() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();

  let paths = vec![PathWithOutputs {
    path: eval.store.parse_store_path(Path::new(
      "/home/jude/.code/rust/rnix/target/debug/build/rnix-9da108a7a9a129ee/out/nix/store/\
       l10kwqwyin8d008l710rmiprxp2dgvq9-hello-2.10.drv",
    ))?,
    outputs: std::iter::once("out".to_string()).collect(),
  }];

  info!("test: attempting to build out paths {:?}", paths);

  eval.store.build_paths(paths)?;

  Ok(())
}
