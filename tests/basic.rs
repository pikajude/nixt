#[macro_use] extern crate log;
use rix::{eval::Eval, path::PathWithOutputs, util::*};
use std::path::Path;

#[test]
fn test_basic_build() -> Result<()> {
  std::env::set_var("_NIX_TEST", "1");
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();

  let drv_path = get_derivation(&eval)?;

  let paths = vec![PathWithOutputs {
    path: eval.store.parse_store_path(Path::new(drv_path))?,
    outputs: std::iter::once("out".to_string()).collect(),
  }];

  info!("test: attempting to build out paths {:?}", paths);

  eval.store.build_paths(paths)?;

  Ok(())
}

fn get_derivation(eval: &Eval) -> Result<&str> {
  let expr = eval.load_inline(
    "(import <nixpkgs> {
    overlays = [];
  }).stdenv.bootstrapTools.outPath",
  )?;
  match eval.value_with_context_of(expr) {
    Ok((_, ctx)) => {
      if let Some(drvpath) = ctx
        .iter()
        .find(|x| x.starts_with("!out!"))
        .and_then(|x| x.strip_prefix("!out!"))
      {
        Ok(drvpath)
      } else {
        panic!("incorrect derivation")
      }
    }
    Err(e) => {
      eval.print_error(e)?;
      panic!("eval failed")
    }
  }
}
