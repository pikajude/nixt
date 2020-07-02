use rnix::eval::Eval;

#[tokio::main]
async fn main() {
  let mut eval = Eval::new();

  let thunk_id = eval
    .load(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/simple.nix"))
    .await
    .unwrap();

  assert!(eval.force(thunk_id).is_ok());
}
