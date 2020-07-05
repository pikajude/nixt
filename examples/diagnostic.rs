use rnix::eval::Eval;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
  pretty_env_logger::init();

  let eval = Eval::new()?;

  let thunk = eval.load_inline("import <nixpkgs/lib>").await?;

  match eval.forced(thunk).await {
    Ok(v) => eprintln!("{:?}", v),
    Err(e) => eval.bail(e),
  }

  Ok(())
}
