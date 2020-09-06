#![feature(map_first_last)]

#[macro_use] extern crate log;
use rix::{eval::Eval, path::PathWithOutputs, util::*};
use std::path::Path;

#[test]
fn test_basic_build() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();

  let paths = vec![PathWithOutputs {
    path: eval.store.parse_store_path(Path::new(
      "/home/jude/.code/rust/rix/target/debug/build/rix-fd8dabb2b740078e/out/nix/store/\
       4ym3mblphhnk4n39kdc9zndwh7qa8j2q-hello-2.10.drv",
    ))?,
    outputs: std::iter::once("out".to_string()).collect(),
  }];

  info!("test: attempting to build out paths {:?}", paths);

  eval.store.build_paths(paths)?;

  Ok(())
}
