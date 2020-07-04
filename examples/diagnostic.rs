use rnix::eval::Eval;

fn main() -> anyhow::Result<()> {
  pretty_env_logger::init();

  let mut eval = Eval::new()?;

  let thunk = eval.load_inline("import <nixpkgs> {}")?;

  match eval.forced(thunk) {
    Ok(v) => eprintln!("{:?}", v),
    Err(e) => eval.bail(e),
  }

  Ok(())
}
