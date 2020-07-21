pub fn break_str(s: &str, pattern: char) -> Option<(&str, &str)> {
  let indexof = s.find(pattern)?;

  Some((&s[..indexof], &s[indexof + pattern.len_utf8()..]))
}
