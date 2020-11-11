use super::*;
use std::fmt::Debug;

#[derive(Clone)]
pub enum PrimopFn {
  Dynamic(Arc<dyn Fn(&Eval, &ValueRef) -> Result<ValueRef>>),
  Static(fn(&Eval, &ValueRef) -> Result<ValueRef>),
}

#[derive(Clone)]
pub struct Primop {
  pub name: &'static str,
  pub fun: PrimopFn,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "<primop {}>", &self.name)
  }
}

impl Primop {
  pub fn one(name: &'static str, f: fn(&Eval, &ValueRef) -> Result<ValueRef>) -> ValueRef {
    arc(Value::Primop(Self {
      name,
      fun: PrimopFn::Static(f),
    }))
  }
}

#[macro_export]
macro_rules! op2 {
  ($name:literal, $op:expr) => {
    ::std::sync::Arc::new(::parking_lot::RwLock::new(
      $crate::eval2::value::Value::Primop($crate::eval2::primop::Primop {
        name: $name,
        fun: $crate::eval2::primop::PrimopFn::Static(move |_, t1| {
          let t1 = Arc::clone(t1);
          Ok(::std::sync::Arc::new(::parking_lot::RwLock::new(
            $crate::eval2::value::Value::Primop($crate::eval2::primop::Primop {
              name: concat!($name, "-app"),
              fun: $crate::eval2::primop::PrimopFn::Dynamic(Arc::new(move |eval, t2| {
                $op(eval, &t1, t2)
              })),
            }),
          )))
        }),
      }),
    ))
  };
}

#[macro_export]
macro_rules! op3 {
  ($name:literal, $op:expr) => {
    ::std::sync::Arc::new(::parking_lot::RwLock::new(
      $crate::eval2::value::Value::Primop($crate::eval2::primop::Primop {
        name: $name,
        fun: $crate::eval2::primop::PrimopFn::Static(move |_, t1| {
          let t1 = Arc::clone(t1);
          Ok(::std::sync::Arc::new(::parking_lot::RwLock::new(
            $crate::eval2::value::Value::Primop($crate::eval2::primop::Primop {
              name: concat!($name, "-app"),
              fun: $crate::eval2::primop::PrimopFn::Dynamic(Arc::new(move |_, t2| {
                let t1 = Arc::clone(&t1);
                let t2 = Arc::clone(t2);
                Ok(::std::sync::Arc::new(::parking_lot::RwLock::new(
                  $crate::eval2::value::Value::Primop($crate::eval2::primop::Primop {
                    name: concat!($name, "-app"),
                    fun: $crate::eval2::primop::PrimopFn::Dynamic(Arc::new(move |eval, t3| {
                      $op(eval, &t1, &t2, t3)
                    })),
                  }),
                )))
              })),
            }),
          )))
        }),
      }),
    ))
  };
}
