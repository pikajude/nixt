use rnix::eval::Eval;

fn main() -> anyhow::Result<()> {
  let mut eval = Eval::new();

  let thunk = eval.load_inline(
    "({ x }: x) 3"
    // r#"derivation {
    //   name = "test-derivation";
    // }"#,
  )?;

  match eval.forced(thunk) {
    Ok(v) => eprintln!("{:?}", v),
    Err(e) => eval.bail(e),
  }

  Ok(())
}
