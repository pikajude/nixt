#[macro_use] extern crate log;

use rnix::eval::Eval;

#[async_std::main]
async fn main() -> rnix::util::Result<()> {
  pretty_env_logger::init();

  let eval = Eval::new()?;
  let mut rl = rustyline::Editor::<()>::with_config(
    rustyline::Config::builder().auto_add_history(true).build(),
  );

  let history_file = dirs_next::cache_dir().map(|x| x.join("nix-repl"));

  if let Some(f) = history_file.as_ref() {
    rl.load_history(f)
      .unwrap_or_else(|e| info!("Unable to load nix-repl history: {}", e))
  }

  while let Ok(line) = rl.readline("> ") {
    match eval.load_inline(line) {
      Ok(expr) => match eval.value_of(expr).await {
        Ok(e) => eprintln!("{:?}", e),
        Err(x) => eval.print_error(x)?,
      },
      Err(e) => eprintln!("{}", e),
    }
  }

  history_file.map(|x| rl.save_history(&x));

  Ok(())
}
