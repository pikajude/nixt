use rnix::{eval::Eval, util::*};
use std::path::Path;

#[test]
#[ntest::timeout(10000)]
fn test_basic_build() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let store = Eval::new().unwrap().store;

  store
    .goal_for(Path::new(concat!(
      env!("OUT_DIR"),
      "/nix/store/ygmgkr3zwl2crvk7injy4njjfwc32cq6-hello-2.10.drv"
    )))?
    .local_build()
}
