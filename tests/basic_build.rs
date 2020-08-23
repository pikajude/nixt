#![feature(map_first_last)]

#[macro_use] extern crate log;
use rnix::{eval::Eval, path::PathWithOutputs, util::*};
use std::path::Path;

#[test]
#[ignore]
fn test_basic_build() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();
  let expr = eval.load_inline(
    "(import <nixpkgs> {
        overlays = [];
      }).hello.outPath",
  )?;

  match eval.value_with_context_of(expr) {
    Ok((_, out_paths)) => {
      assert!(out_paths.len() == 1);
      let out_path = out_paths
        .clone()
        .pop_first()
        .expect("out_paths should have at exactly one item");
      let paths = vec![PathWithOutputs {
        path: eval.store.parse_store_path(Path::new(
          out_path
            .strip_prefix("!out!")
            .expect("out path did not have expected prefix"),
        ))?,
        outputs: std::iter::once("out".to_string()).collect(),
      }];

      info!("test: attempting to build out paths {:?}", paths);

      eval.store.build_paths(paths)?;
    }
    Err(e) => {
      eval.print_error(e)?;
      panic!("eval failed")
    }
  }

  Ok(())
}
