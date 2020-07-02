pub mod source_tree;
pub extern crate syntax;

#[cfg(test)]
mod tests {
  #[tokio::test]
  async fn test_parse() {
    let expr = crate::source_tree::Source::new()
      .load_file(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/simple.nix"))
      .await;

    assert!(expr.is_ok());
  }

  #[tokio::test]
  async fn test_nonexistent() {
    let expr = crate::source_tree::Source::new()
      .load_file(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/nonexistent.nix"
      ))
      .await;

    assert!(expr.is_err());
  }
}
