use crate::{
  eval::{
    thunk::{Thunk, ThunkCell, ThunkId},
    value::Value,
    Eval,
  },
  util::*,
};

pub fn gen_list(eval: &Eval, generator: ThunkId, len: ThunkId) -> Result<Value> {
  let target_len = eval.value_int_of(len)?;
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

pub fn map_list(eval: &Eval, op: ThunkId, value: ThunkId) -> Result<Value> {
  let thunks = eval.value_list_of(value)?;
  Ok(Value::List(eval.items.alloc_extend(
    thunks.iter().map(|t| Thunk::new(ThunkCell::Apply(op, *t))),
  )))
}

pub fn elem_at(eval: &Eval, list: ThunkId, index: ThunkId) -> Result<Value> {
  let ix = eval.value_int_of(index)?;
  let list = eval.value_list_of(list)?;
  if ix < 0 || ix >= list.len() as i64 {
    bail!("list index {} is out of bounds", ix);
  }
  Ok(Value::Ref(list[ix as usize]))
}

pub fn elem(eval: &Eval, thing: ThunkId, list: ThunkId) -> Result<Value> {
  let thing = eval.value_of(thing)?;
  for item in eval.value_list_of(list)? {
    if crate::eval::operators::eval_eq(eval, thing, eval.value_of(*item)?)? {
      return Ok(Value::Bool(true));
    }
  }
  Ok(Value::Bool(false))
}

pub fn head(eval: &Eval, list: ThunkId) -> Result<Value> {
  Ok(Value::Ref(
    eval
      .value_list_of(list)?
      .first()
      .copied()
      .ok_or_else(|| anyhow::anyhow!("builtins.head: empty list"))?,
  ))
}

pub fn tail(eval: &Eval, list: ThunkId) -> Result<Value> {
  let ls = eval.value_list_of(list)?;
  if ls.is_empty() {
    bail!("builtins.tail: empty list")
  } else {
    Ok(Value::List(ls[1..].to_vec()))
  }
}

pub fn filter(eval: &Eval, filter: ThunkId, list: ThunkId) -> Result<Value> {
  let items = eval.value_list_of(list)?;
  let mut out = vec![];
  for item in items {
    match eval.step_fn(filter, *item)? {
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

pub fn concat_lists(eval: &Eval, list: ThunkId) -> Result<Value> {
  let mut all = vec![];
  for list in eval.value_list_of(list)? {
    for item in eval.value_list_of(*list)? {
      all.push(*item);
    }
  }
  Ok(Value::List(all))
}

pub fn foldl_strict(eval: &Eval, oper: ThunkId, seed: ThunkId, list: ThunkId) -> Result<Value> {
  eval.expect_fn(oper)?;
  let items = eval.value_list_of(list)?;
  if items.is_empty() {
    let _ = eval.value_of(seed)?;
    Ok(Value::Ref(seed))
  } else {
    let mut cur_item = seed;
    for (i, thunk) in items.iter().enumerate() {
      let acc_fn = eval.step_fn(oper, cur_item)?;
      let acc_thunk = eval.new_value(acc_fn);
      let next_item = eval.step_fn(acc_thunk, *thunk)?;
      if i + 1 == items.len() {
        return Ok(next_item);
      } else {
        cur_item = eval.new_value(next_item);
      }
    }
    unreachable!()
  }
}
