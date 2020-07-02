use codespan_reporting::term::{emit, Config};
use rnix::{source_tree::Source, syntax::ParseError};
use termcolor::{ColorChoice, StandardStream};

#[tokio::main]
async fn main() {
  let mut tree = Source::new();

  let parse = tree
    .load_file(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/error.nix"))
    .await
    .unwrap_err();

  let mut stderr = StandardStream::stderr(ColorChoice::Auto);

  if let Some(p) = parse.downcast_ref::<ParseError>() {
    emit(&mut stderr, &Config::default(), &tree, &p.diagnose()).unwrap();
  }
}
