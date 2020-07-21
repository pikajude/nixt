use crate::{
  bail,
  error::Result,
  thunk::{Thunk, ThunkCell, ThunkId},
  value::Value,
  Eval,
};

pub async fn gen_list(eval: &Eval, generator: ThunkId, len: ThunkId) -> Result<Value> {
  let target_len = eval.value_int_of(len).await?;
  let target_len = if target_len < 0 {
    bail!("cannot create a list of size {}", target_len);
  } else {
    target_len as usize
  };
  let mut out_list = vec![];
  for val in 0..target_len {
    let fn_arg = eval.new_value(Value::Int(val as _));
    out_list.push(
      eval
        .items
        .alloc(Thunk::new(ThunkCell::Apply(generator, fn_arg))),
    );
  }
  Ok(Value::List(out_list))
}

pub async fn map_list(eval: &Eval, op: ThunkId, value: ThunkId) -> Result<Value> {
  let thunks = eval.value_list_of(value).await?;
  Ok(Value::List(eval.items.alloc_extend(
    thunks.iter().map(|t| Thunk::new(ThunkCell::Apply(op, *t))),
  )))
}

pub async fn elem_at(eval: &Eval, list: ThunkId, index: ThunkId) -> Result<Value> {
  let ix = eval.value_int_of(index).await?;
  let list = eval.value_list_of(list).await?;
  if ix < 0 || ix >= list.len() as i64 {
    bail!("list index {} is out of bounds", ix);
  }
  Ok(Value::Ref(list[ix as usize]))
}

pub async fn elem(eval: &Eval, thing: ThunkId, list: ThunkId) -> Result<Value> {
  let thing = eval.value_of(thing).await?;
  for item in eval.value_list_of(list).await? {
    if crate::operators::eval_eq(eval, thing, eval.value_of(*item).await?).await? {
      return Ok(Value::Bool(true));
    }
  }
  Ok(Value::Bool(false))
}

pub async fn head(eval: &Eval, list: ThunkId) -> Result<Value> {
  Ok(Value::Ref(
    eval
      .value_list_of(list)
      .await?
      .first()
      .copied()
      .ok_or_else(|| anyhow::anyhow!("builtins.head: empty list"))?,
  ))
}

pub async fn tail(eval: &Eval, list: ThunkId) -> Result<Value> {
  let ls = eval.value_list_of(list).await?;
  if ls.is_empty() {
    bail!("builtins.tail: empty list")
  } else {
    Ok(Value::List(ls[1..].to_vec()))
  }
}

pub async fn filter(eval: &Eval, filter: ThunkId, list: ThunkId) -> Result<Value> {
  let items = eval.value_list_of(list).await?;
  let mut out = vec![];
  for item in items {
    match eval.step_fn(filter, *item).await? {
      Value::Bool(b) => {
        if b {
          out.push(*item);
        }
      }
      v => bail!("unexpected type: expected bool, got {}", v.typename()),
    }
  }
  Ok(Value::List(out))
}

pub async fn concat_lists(eval: &Eval, list: ThunkId) -> Result<Value> {
  let mut all = vec![];
  for list in eval.value_list_of(list).await? {
    for item in eval.value_list_of(*list).await? {
      all.push(*item);
    }
  }
  Ok(Value::List(all))
}
