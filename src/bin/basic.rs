#[macro_use] extern crate log;
use rix::{eval::Eval, path::PathWithOutputs, util::*};
use std::path::Path;

fn main() -> Result<()> {
  std::env::set_var("_NIX_TEST", "1");

  let eval = Eval::new().unwrap();

  let drv_path = get_derivation(&eval)?;

  let paths = vec![PathWithOutputs {
    path: eval.store.parse_store_path(Path::new(&*drv_path))?,
    outputs: std::iter::once("out".to_string()).collect(),
  }];

  info!("test: attempting to build out paths {:?}", paths);

  eval.store.build_paths(paths)?;

  Ok(())
}

fn get_derivation(eval: &Eval) -> Result<String> {
  if let Ok(x) = std::env::var("BASIC_TEST_DRV") {
    if std::fs::metadata(&x).is_ok() {
      return Ok(x);
    }
  }

  let expr = eval.load_inline(
    "(import <nixpkgs> {
    overlays = [];
  }).rustc.outPath",
  )?;
  match eval.value_with_context_of(expr) {
    Ok((_, ctx)) => {
      if let Some(drvpath) = ctx
        .iter()
        .find(|x| x.starts_with("!out!"))
        .and_then(|x| x.strip_prefix("!out!"))
      {
        eprintln!("export BASIC_TEST_DRV={}", drvpath);
        Ok(drvpath.to_string())
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
