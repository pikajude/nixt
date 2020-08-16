use super::{thunk::ThunkId, value::Value, Eval};
use crate::util::*;
use futures::{future::BoxFuture, Future};
use std::fmt::{self, Debug};

pub enum Op {
  Static(fn(&Eval, ThunkId) -> Result<Value>),
  Async(Box<dyn Fn(&Eval, ThunkId) -> BoxFuture<Result<Value>> + Send + Sync>),
}

static_assertions::assert_impl_all!(Op: Send, Sync);

pub struct Primop {
  pub name: &'static str,
  pub op: Op,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<primop {}>", &self.name)
  }
}

impl Primop {
  pub fn single_async<F: Future<Output = Result<Value>> + Send + 'static>(
    name: &'static str,
    f: fn(&Eval, ThunkId) -> F,
  ) -> Value {
    Value::Primop(Self {
      name,
      op: Op::Async(Box::new(move |eval, thunkid| Box::pin(f(eval, thunkid)))),
    })
  }
}

#[macro_export]
macro_rules! primop_inline {
  ($name:expr, |$($p:pat),*| $t:tt) => {
    primop!($name, |$($p),*| async move $t)
  };
}

#[macro_export]
macro_rules! primop {
  ($name:literal, $op:expr$(,)?) => {
    $crate::eval::value::Value::Primop($crate::eval::primop::Primop {
      name: $name,
      op: $crate::eval::primop::Op::Async(Box::new(move |eval, t| Box::pin($op(eval, t)))),
    })
  };
}

#[macro_export]
macro_rules! primop2 {
  ($name:literal, $op:expr$(,)?) => {
    $crate::eval::value::Value::Primop($crate::eval::primop::Primop {
      name: $name,
      op: $crate::eval::primop::Op::Static(move |_, t1| {
        Ok($crate::eval::value::Value::Primop(
          $crate::eval::primop::Primop {
            name: concat!($name, "-app"),
            op: $crate::eval::primop::Op::Async(Box::new(move |eval, t2| {
              Box::pin($op(eval, t1, t2))
            })),
          },
        ))
      }),
    })
  };
}

#[macro_export]
macro_rules! primop3 {
  ($name:literal, $op:expr$(,)?) => {
    $crate::eval::value::Value::Primop($crate::eval::primop::Primop {
      name: $name,
      op: $crate::eval::primop::Op::Static(move |_, t1| {
        Ok($crate::eval::value::Value::Primop(
          $crate::eval::primop::Primop {
            name: concat!($name, "-app"),
            op: $crate::eval::primop::Op::Async(Box::new(move |_, t2| {
              Box::pin(async move {
                Ok($crate::eval::value::Value::Primop(
                  $crate::eval::primop::Primop {
                    name: concat!($name, "-app"),
                    op: $crate::eval::primop::Op::Async(Box::new(move |eval, t3| {
                      Box::pin($op(eval, t1, t2, t3))
                    })),
                  },
                ))
              })
            })),
          },
        ))
      }),
    })
  };
}
