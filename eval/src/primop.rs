use super::{thunk::ThunkId, value::Value, Eval};
use nix_util::*;
use std::fmt::{self, Debug};

pub enum Op {
  Dynamic(Box<dyn Fn(&Eval, ThunkId) -> Result<Value>>),
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
  ($name:expr, $e:expr) => {
    $crate::primop::Primop::single($name, $e)
  };
}

#[macro_export]
macro_rules! primop {
  ($name:literal, $op:expr) => {
    $crate::primop::Primop::single($name, $op)
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
          op: $crate::primop::Op::Dynamic(Box::new(move |eval, t2| $op(eval, t1, t2))),
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
          op: $crate::primop::Op::Dynamic(Box::new(move |_, t2| {
            Ok($crate::value::Value::Primop($crate::primop::Primop {
              name: concat!($name, "-app"),
              op: $crate::primop::Op::Dynamic(Box::new(move |eval, t3| $op(eval, t1, t2, t3))),
            }))
          })),
        }))
      }),
    })
  };
}
