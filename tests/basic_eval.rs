use rix::{eval::Eval, util::*};

#[test]
fn test_basic_eval() -> Result<()> {
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();
  let expr = eval.load_inline("(import /root/nixpkgs {}).hello.outPath")?;
  match eval.value_with_context_of(expr) {
    Ok(_) => println!("successfully instantiated pkgs.hello"),
    Err(e) => {
      eval.print_error(e)?;
      panic!("eval failed")
    }
  }

  Ok(())
}
