use rnix::{Config, Eval};

fn main() -> rnix::Result<()> {
  let eval = Eval::with_config(Config { trace: true });
  let mut rl = rustyline::Editor::<()>::with_config(
    rustyline::Config::builder().auto_add_history(true).build(),
  );
  while let Ok(line) = rl.readline("> ") {
    let expr = eval.load_inline(line)?;
    match eval.value_of(expr) {
      Ok(e) => eprintln!("{:?}", e),
      Err(x) => eval.print_error(x)?,
    }
  }

  Ok(())
}
