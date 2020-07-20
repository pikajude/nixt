use crate::{bail, error::Result, thunk::ThunkId, value::Value, Eval};

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
