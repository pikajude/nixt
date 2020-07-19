use crate::{error::Result, primop::Primop, thunk::ThunkId, value::Value, Eval};
use syntax::expr::Ident;

async fn remove_attrs(eval: &Eval, attrset: ThunkId, to_remove: ThunkId) -> Result<Value> {
  let mut attrset = eval.value_attrs_of(attrset).await?.clone();
  for attr_name in eval.value_list_of(to_remove).await? {
    let (remove_item, _) = eval.value_str_of(*attr_name).await?;
    attrset.remove(&Ident::from(remove_item));
  }
  Ok(Value::AttrSet(attrset))
}

pub async fn remove_attrs_primop(_: &Eval, lhs: ThunkId) -> Result<Value> {
  Ok(Value::Primop(Primop {
    name: "removeAttrs-app".into(),
    op: Box::new(move |eval, rhs| Box::pin(remove_attrs(eval, lhs, rhs))),
  }))
}
