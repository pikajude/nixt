use crate::{
  bail,
  error::Result,
  thunk::{Thunk, ThunkCell, ThunkId},
  value::{PathSet, Value},
  Eval,
};
use serde_json::{Number, Value as JSON};

pub async fn to_json(eval: &Eval, obj: ThunkId) -> Result<(JSON, PathSet)> {
  let mut paths = PathSet::new();
  let json = to_json_impl(eval, obj, &mut paths).await?;
  Ok((json, paths))
}

pub async fn to_json_primop(eval: &Eval, obj: ThunkId) -> Result<Value> {
  let (j, p) = to_json(eval, obj).await?;
  Ok(Value::String {
    string: serde_json::to_string(&j).unwrap(),
    context: p,
  })
}

async fn to_json_impl(eval: &Eval, obj: ThunkId, paths: &mut PathSet) -> Result<JSON> {
  Ok(match eval.value_of(obj).await? {
    Value::Null => JSON::Null,
    Value::Int(i) => JSON::Number(Number::from(*i)),
    Value::Float(f) => JSON::Number(Number::from_f64(*f).unwrap()),
    Value::String { string, context } => {
      paths.extend(context.iter().cloned());
      JSON::String(string.into())
    }
    v => bail!("don't know how to convert {} to JSON", v.typename()),
  })
}
