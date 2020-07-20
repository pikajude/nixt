use crate::{
  bail,
  error::Result,
  thunk::{StaticScope, ThunkId},
  value::Value,
  Eval,
};
use syntax::expr::Ident;

pub fn fetch_tarball(eval: &Eval, args: ThunkId) -> Result<Value> {
  match eval.value_of(args)? {
    _ => bail!("fetchTarball expects a URL or an attrset of arguments"),
  }
}
