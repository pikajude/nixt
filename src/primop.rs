use crate::{error::Result, thunk::ThunkId, value::Value, Eval};
use futures::{future::BoxFuture, Future};
use std::fmt::{self, Debug};

pub struct Primop {
  pub name: String,
  pub op: Box<dyn Fn(&Eval, ThunkId) -> BoxFuture<Result<Value>> + Send + Sync>,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<<primop {}>", &self.name)
  }
}

impl Primop {
  pub fn wrap<F, Fut, N>(name: N, op: F) -> Self
  where
    F: for<'r> Fn(&'r Eval, ThunkId) -> Fut + Send + Sync + 'static,
    Fut: Future<Output = Result<Value>> + Send + Sync + 'static,
    N: Into<String>,
  {
    Self {
      name: name.into(),
      op: Box::new(move |eval, thunk| Box::pin(op(eval, thunk))),
    }
  }
}
