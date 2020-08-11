use rnix::{eval::Eval, util::*};

#[cfg_attr(debug_assertions, ignore)]
#[test]
fn test_basic_eval() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();
  let expr = eval.load_inline("(import <nixpkgs> {}).hello.outPath")?;
  match eval.value_with_context_of(expr) {
    Ok((_, context)) => {
      for c in context {
        let drv_path = c.strip_prefix("!out!").unwrap();
        let derivation = eval
          .store
          .parse_derivation(std::path::Path::new(&drv_path), "hello-2.10.drv")?;
        println!("must build: {:?}", derivation);
      }
    }
    Err(e) => {
      eval.print_error(e)?;
      panic!("eval failed")
    }
  }

  Ok(())
}
