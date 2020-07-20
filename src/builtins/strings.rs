use crate::{bail, error::Result, thunk::ThunkId, value::Value, Eval};
use std::collections::BTreeSet;

pub async fn substring(
  eval: &Eval,
  start: ThunkId,
  len: ThunkId,
  string: ThunkId,
) -> Result<Value> {
  let (s, ctx) = eval.value_str_of(string).await?;
  let start = eval.value_int_of(start).await?;
  if start < 0 {
    bail!("first argument to `substring' must be >= 0");
  }
  let start = start as usize;
  let len = eval.value_int_of(len).await?;
  let actual_end = std::cmp::min(start + (std::cmp::max(0, len) as usize), s.len());
  Ok(Value::String {
    string: s[start..actual_end].to_string(),
    context: ctx.clone(),
  })
}

pub async fn coerce_to_string(eval: &Eval, obj: ThunkId) -> Result<Value> {
  let v = eval.value_of(obj).await?;
  Ok(match v {
    Value::Path(p) => Value::string_bare(p.display().to_string()),
    Value::String { string, context } => Value::String {
      string: string.clone(),
      context: context.clone(),
    },
    Value::Int(i) => Value::string_bare(i.to_string()),
    _ => bail!("not handled, coercing to string: {}", v.typename()),
  })
}

pub async fn concat_strings_sep(eval: &Eval, sep: ThunkId, strings: ThunkId) -> Result<Value> {
  let strings = eval.value_list_of(strings).await?;
  if strings.is_empty() {
    return Ok(Value::string_bare(""));
  }
  let mut all_ctx = BTreeSet::new();
  let mut output = String::new();
  let (sep, c) = eval.value_str_of(sep).await?;
  all_ctx.extend(c.iter().cloned());

  for (ix, s) in strings.iter().enumerate() {
    if ix > 0 {
      output.push_str(sep);
    }
    let (s1, ctx) = eval.value_str_of(*s).await?;
    output.push_str(s1);
    all_ctx.extend(ctx.iter().cloned());
  }

  Ok(Value::String {
    string: output,
    context: all_ctx,
  })
}
