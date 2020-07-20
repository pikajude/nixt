use crate::{error::Result, thunk::ThunkId, value::Value, Eval};
use futures::{future::BoxFuture, Future};
use std::fmt::{self, Debug};

pub enum Op {
  Async(Box<dyn Fn(&Eval, ThunkId) -> BoxFuture<Result<Value>> + Send + Sync>),
  Sync(fn(&Eval, ThunkId) -> Result<Value>),
}

pub struct Primop {
  pub name: &'static str,
  pub op: Op,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<<primop {}>", &self.name)
  }
}

#[macro_export]
macro_rules! primop_inline {
  ($name:expr, |$($p:pat),*| $e:expr) => {
    $crate::thunk::Thunk::complete($crate::value::Value::Primop($crate::primop::Primop {
      name: $name,
      op: $crate::primop::Op::Async(Box::new(move |$($p),*| Box::pin(async move { $e })))
    }))
  }
}

#[macro_export]
macro_rules! primop {
  ($name:expr, $op:expr) => {
    $crate::thunk::Thunk::complete($crate::value::Value::Primop($crate::primop::Primop {
      name: $name,
      op: $crate::primop::Op::Async(Box::new(move |eval, thunk| Box::pin($op(eval, thunk)))),
    }))
  };
}

#[macro_export]
macro_rules! primop2 {
  ($name:literal, $op:expr) => {
    $crate::thunk::Thunk::complete($crate::value::Value::Primop($crate::primop::Primop {
      name: $name,
      op: $crate::primop::Op::Sync(move |_, t1| {
        Ok($crate::value::Value::Primop($crate::primop::Primop {
          name: concat!($name, "-app"),
          op: $crate::primop::Op::Async(Box::new(move |eval, t2| Box::pin($op(eval, t1, t2)))),
        }))
      }),
    }))
  };
}
