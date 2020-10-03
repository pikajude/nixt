use super::*;

macro_rules! assert_eval {
  ($l:literal, $p:pat) => {{
    let eval = Eval::new().unwrap();
    assert_matches::assert_matches!(eval.value_of(eval.load_inline($l)?), $p)
  }};
}

#[test]
fn test_rec_order() -> Result<()> {
  assert_eval!(
    r#"rec { ${"${b}a"} = 1; ${"b"} = "a"; }.aa"#,
    Ok(Value::Int(1))
  );
  assert_eval!(r#"rec { ${"a"} = 1; b = a; }.b"#, Ok(Value::Int(1)));
  assert_eval!(r#"rec { inherit (a) b; a.b = 3; }.b"#, Ok(Value::Int(3)));
  assert_eval!(r#"with { a = 1; }; with { a = 2; }; a"#, Ok(Value::Int(2)));
  Ok(())
}

#[test]
fn test_foldl() -> Result<()> {
  let e = Eval::new().unwrap();
  let expr = e.load_inline(r#"builtins.foldl' (x: y: "${x}-${y}") "foo" ["bar" "baz" "qux"]"#)?;
  assert_eq!(e.value_string_of(expr)?, "foo-bar-baz-qux");
  Ok(())
}

#[test]
fn test_replace() -> Result<()> {
  let e = Eval::new().unwrap();
  let expr = e.load_inline(
    r#"
      builtins.replaceStrings ["-" "."] ["_" "_"] "x86_64-unknown-linux-gnu"
    "#,
  )?;
  assert_eq!(e.value_string_of(expr)?, "x86_64_unknown_linux_gnu");
  Ok(())
}

#[test]
fn test_unindent() -> Result<()> {
  let e = Eval::new().unwrap();
  let expr = e.load_inline(
    r#"
      ''
            foo
            bar
            baz
      ''
    "#,
  )?;
  assert_eq!(e.value_string_of(expr)?, "foo\nbar\nbaz\n");
  Ok(())
}
