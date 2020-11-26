use rix::{eval::Eval, prelude::*};

fn main() -> Result<()> {
  rix::logger::init()?;

  let e = Eval::default();
  e.create_base_env()?;

  eprintln!(
    "{:?}",
    e.eval_inline(
      "import /home/jude/.code/nix/pkgs/default.nix {
      overlays = [];
    }"
    )
    .or_exit()
    .debug()
  );
  Ok(())
}
