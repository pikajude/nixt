use crate::{
  bail,
  error::Result,
  thunk::ThunkId,
  value::{PathSet, Value},
  Eval,
};
use std::collections::BTreeSet;

pub fn substring(eval: &Eval, start: ThunkId, len: ThunkId, string: ThunkId) -> Result<Value> {
  let (ctx, s) = coerce_new_string(eval, string, false, false)?;
  let start = eval.value_int_of(start)?;
  if start < 0 {
    bail!("first argument to `substring' must be >= 0");
  }
  let start = start as usize;
  let len = eval.value_int_of(len)?;
  let actual_end = std::cmp::min(start + (std::cmp::max(0, len) as usize), s.len());
  Ok(Value::String {
    string: s[start..actual_end].to_string(),
    context: ctx,
  })
}

pub fn prim_to_string(eval: &Eval, obj: ThunkId) -> Result<Value> {
  let mut ctx = PathSet::new();
  Ok(Value::String {
    string: coerce_to_string(eval, obj, &mut ctx, true, false)?,
    context: ctx,
  })
}

pub fn coerce_new_string(
  eval: &Eval,
  obj: ThunkId,
  extended: bool,
  copy_to_store: bool,
) -> Result<(PathSet, String)> {
  let mut p = PathSet::new();
  let string = coerce_to_string(eval, obj, &mut p, extended, copy_to_store)?;
  Ok((p, string))
}

pub fn coerce_to_string(
  eval: &Eval,
  obj: ThunkId,
  ctx: &mut PathSet,
  extended: bool,
  copy_to_store: bool,
) -> Result<String> {
  let v = eval.value_of(obj)?;
  Ok(match v {
    Value::Path(p) => p.display().to_string(),
    Value::String { string, context } => {
      ctx.extend(context.iter().cloned());
      string.clone()
    }
    Value::Int(i) => i.to_string(),
    Value::Bool(b) if extended => {
      if *b {
        "1".into()
      } else {
        String::new()
      }
    }
    Value::List(items) if extended => {
      let mut output = String::new();
      for (i, item) in items.iter().enumerate() {
        if i > 0 {
          output.push(' ');
        }
        output.push_str(&coerce_to_string(
          eval,
          *item,
          ctx,
          extended,
          copy_to_store,
        )?);
      }
      output
    }
    v => bail!("cannot convert {} to a string", v.typename()),
  })
}

pub fn concat_strings_sep(eval: &Eval, sep: ThunkId, strings: ThunkId) -> Result<Value> {
  let strings = eval.value_list_of(strings)?;
  if strings.is_empty() {
    return Ok(Value::string_bare(""));
  }
  let mut all_ctx = BTreeSet::new();
  let mut output = String::new();
  let (sep, pset) = eval.value_with_context_of(sep)?;
  all_ctx.extend(pset.iter().cloned());

  for (ix, s) in strings.iter().enumerate() {
    if ix > 0 {
      output.push_str(sep);
    }
    output.push_str(&coerce_to_string(eval, *s, &mut all_ctx, false, false)?);
  }

  Ok(Value::String {
    string: output,
    context: all_ctx,
  })
}
