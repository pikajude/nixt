use crate::{
  bail,
  error::Result,
  primop::Primop,
  primop3,
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
    let fn_arg = eval.items.alloc(Thunk::complete(Value::Int(val as _)));
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

pub async fn concat_lists(eval: &Eval, list: ThunkId) -> Result<Value> {
  let mut all = vec![];
  for list in eval.value_list_of(list).await? {
    for item in eval.value_list_of(*list).await? {
      all.push(*item);
    }
  }
  Ok(Value::List(all))
}
