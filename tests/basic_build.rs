use rnix::{eval::Eval, path::PathWithOutputs, util::*};
use std::path::Path;

#[test]
fn test_basic_build() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let store = Eval::new().unwrap().store;

  let paths = vec![PathWithOutputs {
    path: store.parse_store_path(Path::new(concat!(
      env!("OUT_DIR"),
      "/nix/store/ygmgkr3zwl2crvk7injy4njjfwc32cq6-hello-2.10.drv"
    )))?,
    outputs: std::iter::once("out".to_string()).collect(),
  }];

  store.build_paths(paths)?;

  Ok(())
}
