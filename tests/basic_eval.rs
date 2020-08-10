use rnix::{eval::Eval, util::*};

#[cfg_attr(debug_assertions, ignore)]
#[test]
fn test_basic_eval() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();
  let expr = eval.load_inline("(import <nixpkgs> {}).hello.outPath")?;
  match eval.value_with_context_of(expr) {
    Ok((outpath, _)) => {
      eval
        .store
        .build_paths(vec![eval.store.parse_path_with_outputs(outpath)?])?;
      println!("successfully build path {}", outpath);
    }
    Err(e) => {
      eval.print_error(e)?;
      panic!("eval failed")
    }
  }

  Ok(())
}
