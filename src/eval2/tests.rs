use super::*;

macro_rules! assert_eval {
  ($l:literal, $p:pat) => {{
    let eval = Eval::new();
    assert_matches::assert_matches!(&*eval.value(&eval.load_inline($l)?)?, $p)
  }};
}

#[test]
fn test_rec_order() -> Result<()> {
  assert_eval!(r#"rec { ${"${b}a"} = 1; ${"b"} = "a"; }.aa"#, Value::Int(1));
  assert_eval!(r#"rec { ${"a"} = 1; b = a; }.b"#, Value::Int(1));
  assert_eval!(r#"rec { inherit (a) b; a.b = 3; }.b"#, Value::Int(3));
  assert_eval!(r#"with { a = 1; }; with { a = 2; }; a"#, Value::Int(2));
  Ok(())
}

#[test]
fn test_foldl() -> Result<()> {
  let e = Eval::new();
  let expr = e.load_inline(r#"foldl' (x: y: "${x}-${y}") "foo" ["bar" "baz" "qux"]"#)?;
  assert_eq!(&*e.value_string_of(&expr)?, "foo-bar-baz-qux");
  Ok(())
}
