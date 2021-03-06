use crate::{
  eval::{
    context::StaticScope,
    thunk::ThunkId,
    value::{PathSet, Value},
    Eval,
  },
  syntax::expr::Ident,
  util::*,
};
use serde_json::{Map, Number, Value as JSON};

pub fn to_json(eval: &Eval, obj: ThunkId) -> Result<(JSON, PathSet)> {
  let mut paths = PathSet::new();
  let json = to_json_impl(eval, obj, &mut paths)?;
  Ok((json, paths))
}

pub fn to_json_primop(eval: &Eval, obj: ThunkId) -> Result<Value> {
  let (j, p) = to_json(eval, obj)?;
  Ok(Value::String {
    string: serde_json::to_string(&j).unwrap(),
    context: p,
  })
}

fn to_json_impl(eval: &Eval, obj: ThunkId, paths: &mut PathSet) -> Result<JSON> {
  Ok(match eval.value_of(obj)? {
    Value::Null => JSON::Null,
    Value::Int(i) => JSON::Number(Number::from(*i)),
    Value::Float(f) => JSON::Number(Number::from_f64(*f).unwrap()),
    Value::String { string, context } => {
      paths.extend(context.iter().cloned());
      JSON::String(string.into())
    }
    Value::List(items) => JSON::Array(
      items
        .iter()
        .map(|x| to_json_impl(eval, *x, paths))
        .collect::<Result<Vec<_>>>()?,
    ),
    Value::AttrSet(a) => {
      warn!("unimplemented: attrset.toString()");

      if let Some(op) = a.get(&"outPath".into()) {
        return to_json_impl(eval, *op, paths);
      }

      let mut m = Map::new();
      for (k, v) in a {
        m.insert(k.to_string(), to_json_impl(eval, *v, paths)?);
      }
      JSON::Object(m)
    }
    v => bail!("don't know how to convert {} to JSON", v.typename()),
  })
}

pub fn from_json(eval: &Eval, string: ThunkId) -> Result<Value> {
  let json_string = eval.value_string_of(string)?;
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
