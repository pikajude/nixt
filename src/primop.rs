use crate::{error::Result, eval::Eval, thunk::ThunkId, value::Value};
use futures::future::BoxFuture;
use std::fmt::{self, Debug};

pub enum Op {
  Async(Box<dyn Fn(&Eval, ThunkId) -> BoxFuture<Result<Value>> + Send + Sync>),
  Static(fn(&Eval, ThunkId) -> Result<Value>),
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

impl Primop {
  pub fn single(name: &'static str, f: fn(&Eval, ThunkId) -> Result<Value>) -> Value {
    Value::Primop(Self {
      name,
      op: Op::Static(f),
    })
  }
}

#[macro_export]
macro_rules! primop_inline {
  ($name:expr, |$($p:pat),*| $e:expr) => {
    $crate::value::Value::Primop($crate::primop::Primop {
      name: $name,
      op: $crate::primop::Op::Async(Box::new(move |$($p),*| Box::pin(async move { $e })))
    })
  }
}

#[macro_export]
macro_rules! primop_async {
  ($name:literal, $op:expr) => {
    $crate::value::Value::Primop(Primop {
      name: $name,
      op: $crate::primop::Op::Async(Box::new(move |e, i| Box::pin($op(e, i)))),
    })
  };
}

#[macro_export]
macro_rules! primop2 {
  ($name:literal, $op:expr) => {
    $crate::value::Value::Primop($crate::primop::Primop {
      name: $name,
      op: $crate::primop::Op::Static(move |_, t1| {
        Ok($crate::value::Value::Primop($crate::primop::Primop {
          name: concat!($name, "-app"),
          op: $crate::primop::Op::Async(Box::new(move |eval, t2| Box::pin($op(eval, t1, t2)))),
        }))
      }),
    })
  };
}

#[macro_export]
macro_rules! primop3 {
  ($name:literal, $op:expr) => {
    $crate::value::Value::Primop($crate::primop::Primop {
      name: $name,
      op: $crate::primop::Op::Static(move |_, t1| {
        Ok($crate::value::Value::Primop($crate::primop::Primop {
          name: concat!($name, "-app"),
          op: $crate::primop::Op::Async(Box::new(move |_, t2| {
            Box::pin(async move {
              Ok($crate::value::Value::Primop($crate::primop::Primop {
                name: concat!($name, "-app"),
                op: $crate::primop::Op::Async(Box::new(move |eval, t3| {
                  Box::pin($op(eval, t1, t2, t3))
                })),
              }))
            })
          })),
        }))
      }),
    })
  };
}
