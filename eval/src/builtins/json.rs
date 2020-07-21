use crate::{
  thunk::{StaticScope, ThunkId},
  value::{PathSet, Value},
  Eval,
};
use nix_util::*;
use serde_json::{Map, Number, Value as JSON};
use syntax::expr::Ident;

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

pub async fn from_json(eval: &Eval, string: ThunkId) -> Result<Value> {
  let json_string = eval.value_string_of(string).await?;
  json_to_value(eval, serde_json::from_str(json_string)?)
}

fn json_to_value(eval: &Eval, obj: JSON) -> Result<Value> {
  match obj {
    JSON::Object(m) => obj_to_value(eval, m),
    JSON::String(s) => Ok(Value::string_bare(s)),
    JSON::Null => Ok(Value::Null),
    x => todo!("{:?}", x),
  }
}

fn obj_to_value(eval: &Eval, obj: Map<String, JSON>) -> Result<Value> {
  Ok(Value::AttrSet(
    obj
      .into_iter()
      .map(|(k, v)| Ok((Ident::from(k), eval.new_value(json_to_value(eval, v)?))))
      .collect::<Result<StaticScope>>()?,
  ))
}
