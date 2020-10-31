#[macro_use] extern crate slog_scope;
use rix::{eval::Eval, path::PathWithOutputs, util::*, Store};

fn main() -> Result<()> {
  std::env::set_var("_NIX_TEST", "1");

  let eval = Eval::new().unwrap();

  let drv_path = get_derivation(&eval)?;
  let store_path = eval.store.parse_store_path(drv_path)?;
  let drv = eval.store.read_derivation(&store_path)?;

  let path = PathWithOutputs {
    path: store_path,
    outputs: drv.outputs.keys().cloned().collect(),
  };

  info!("test: attempting to build path {:?}", path);

  eval.store.build_paths(vec![path])?;

  for v in drv.outputs.values() {
    info!("{}", eval.store.print_store_path(&v.path));
  }

  Ok(())
}

fn get_derivation(eval: &Eval) -> Result<String> {
  if let Ok(e) = std::env::var("DERIVATION") {
    return Ok(e);
  }
  let expr = eval.load_inline(
    "(import <nixpkgs> {
    overlays = [];
  }).stdenv.outPath",
  )?;
  match eval.value_with_context_of(expr) {
    Ok((_, ctx)) => {
      if let Some(drvpath) = ctx
        .iter()
        .find(|x| x.starts_with("!out!"))
        .and_then(|x| x.strip_prefix("!out!"))
      {
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
