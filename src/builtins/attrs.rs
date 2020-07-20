use crate::{
  error::Result,
  thunk::{StaticScope, Thunk, ThunkId},
  value::Value,
  Eval,
};
use syntax::expr::Ident;

pub async fn attr_names(eval: &Eval, attrs: ThunkId) -> Result<Value> {
  let mut keys = eval.value_attrs_of(attrs).await?.keys().collect::<Vec<_>>();
  keys.sort_unstable();
  Ok(Value::List(
    eval.items.alloc_extend(
      keys
        .into_iter()
        .map(|s| Thunk::complete(Value::string_bare(s.as_ref()))),
    ),
  ))
}

pub async fn intersect_attrs(eval: &Eval, lhs: ThunkId, rhs: ThunkId) -> Result<Value> {
  let attrs = eval.value_attrs_of(lhs).await?;
  let mut attrs2 = eval.value_attrs_of(rhs).await?.clone();
  attrs2.retain(|key, _| attrs.contains_key(key));
  Ok(Value::AttrSet(attrs2))
}

pub async fn remove_attrs(eval: &Eval, attrset: ThunkId, to_remove: ThunkId) -> Result<Value> {
  let mut attrset = eval.value_attrs_of(attrset).await?.clone();
  for attr_name in eval.value_list_of(to_remove).await? {
    let (remove_item, _) = eval.value_str_of(*attr_name).await?;
    attrset.remove(&Ident::from(remove_item));
  }
  Ok(Value::AttrSet(attrset))
}

pub async fn list_to_attrs(eval: &Eval, list: ThunkId) -> Result<Value> {
  let mut attrs = StaticScope::new();
  let name_sym = Ident::from("name");
  let value_sym = Ident::from("value");
  for obj in eval.value_list_of(list).await? {
    let obj = eval.value_attrs_of(*obj).await?;
    let (name, _) = eval
      .value_str_of(
        obj
          .get(&name_sym)
          .copied()
          .ok_or_else(|| anyhow::anyhow!("Missing attribute name"))?,
      )
      .await?;
    let value = obj
      .get(&value_sym)
      .copied()
      .ok_or_else(|| anyhow::anyhow!("Missing attribute value"))?;
    attrs.insert(Ident::from(name), value);
  }
  Ok(Value::AttrSet(attrs))
}
