use crate::{
  bail,
  error::Result,
  primop::Primop,
  primop3,
  thunk::{Thunk, ThunkCell, ThunkId},
  value::Value,
  Eval,
};
use std::ffi::OsStr;

pub async fn get_env(eval: &Eval, varname: ThunkId) -> Result<Value> {
  let (varname, _) = eval.value_str_of(varname).await?;
  match std::env::var(String::from(varname)) {
    Ok(s) => Ok(Value::string_bare(s)),
    Err(_) => Ok(Value::string_bare("")),
  }
}

pub async fn path_exists(eval: &Eval, path: ThunkId) -> Result<Value> {
  Ok(Value::Bool(match eval.value_of(path).await? {
    Value::String { string, .. } => async_std::fs::metadata(string).await.is_ok(),
    Value::Path(p) => p.metadata().await.is_ok(),
    _ => false,
  }))
}
