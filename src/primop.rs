use crate::{thunk::ThunkId, value::Value, Eval};
use anyhow::Result;
use futures::future::BoxFuture;
use std::fmt::{self, Debug};

pub struct Primop {
  pub name: Option<String>,
  pub op: Box<dyn Fn(&Eval, ThunkId) -> BoxFuture<Result<Value>> + Send + Sync>,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.name {
      Some(x) => write!(f, "<<primop {}>>", x),
      None => write!(f, "<<primop>>"),
    }
  }
}
