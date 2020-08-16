use rnix::{eval::Eval, util::*};

#[async_std::test]
async fn test_basic_eval() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  pretty_env_logger::init();

  let eval = Eval::new().unwrap();
  let expr = eval
    .load_inline("(import <nixpkgs> {}).hello.outPath")
    .await?;
  match eval.value_with_context_of(expr).await {
    Ok(_) => println!("successfully instantiated pkgs.hello"),
    Err(e) => {
      eval.print_error(e).await?;
      panic!("eval failed")
    }
  }

  Ok(())
}
