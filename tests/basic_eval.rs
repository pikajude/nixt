use rnix::{eval::Eval, util::*};

#[cfg_attr(debug_assertions, ignore)]
#[test]
fn test_basic_eval() -> Result<()> {
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();
  let expr = eval.load_inline("(import <nixpkgs> {}).hello.outPath")?;
  match eval.value_of(expr) {
    Ok(x) => eprintln!("{:?}", x),
    Err(e) => {
      eval.print_error(e)?;
      panic!("eval failed")
    }
  }

  Ok(())
}
