use super::context::{Context, StaticScope};
use crate::syntax::expr::{self, ExprRef};
use derivative::Derivative;
use parking_lot::RwLock;
use std::{
  cell::RefCell,
  collections::{BTreeSet, HashSet},
  fmt::Debug,
  sync::Arc,
};

pub type ValueRef = Arc<RwLock<Value>>;

pub(super) fn arc<T>(x: T) -> Arc<RwLock<T>> {
  Arc::new(RwLock::new(x))
}

#[derive(Clone, Debug)]
pub struct StringCtx {
  pub s: String,
  pub context: BTreeSet<String>,
}

#[derive(Clone, Derivative)]
pub enum Value {
  Null,
  Bool(bool),
  Int(i64),
  Float(f64),
  List(Vec<ValueRef>),
  Attrs(StaticScope),
  String(StringCtx),
  Lambda {
    fun: expr::Lambda,
    captures: Box<Context>,
  },
  Primop(super::primop::Primop),
  // mid-eval states that should be extracted into their own type at some point
  Blackhole,
  Thunk(ExprRef, Context),
  Apply(ValueRef, ValueRef),
}

impl Value {
  pub fn should_evaluate(&self) -> bool {
    matches!(self, Self::Thunk { .. } | Self::Apply { .. })
  }

  pub fn is_blackhole(&self) -> bool {
    matches!(self, Self::Blackhole)
  }

  pub fn typename(&self) -> &'static str {
    match self {
      Value::Null => "null",
      Value::Bool(_) => "bool",
      Value::Int(_) => "int",
      Value::Float(_) => "float",
      Value::List(_) => "list",
      Value::Attrs(_) => "attrs",
      Value::String(_) => "string",
      Value::Lambda { .. } => "lambda",
      Value::Primop(_) => "primop",

      Value::Blackhole | Value::Thunk(_, _) | Value::Apply(_, _) => unreachable!(),
    }
  }
}

struct DebugRef<'v> {
  value: &'v ValueRef,
  seen: &'v RefCell<HashSet<*const RwLock<Value>>>,
}

impl Debug for DebugRef<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let ptr = Arc::as_ptr(self.value);
    if self.seen.borrow().contains(&ptr) {
      f.debug_struct("<repeated>").finish()
    } else {
      self.seen.borrow_mut().insert(ptr);
      (DebugValue {
        value: &*self.value.read(),
        seen: self.seen,
      })
      .fmt(f)
    }
  }
}

struct DebugValue<'v> {
  value: &'v Value,
  seen: &'v RefCell<HashSet<*const RwLock<Value>>>,
}

impl Debug for DebugValue<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self.value {
      Value::Blackhole => f.debug_struct("Blackhole").finish(),
      Value::Thunk(e, _) => f.debug_tuple("Thunk").field(e).finish(),
      Value::Apply(lhs, rhs) => {
        let mut m = f.debug_tuple("Apply");
        m.field(&DebugRef {
          value: lhs,
          seen: self.seen,
        });
        m.field(&DebugRef {
          value: rhs,
          seen: self.seen,
        });
        m.finish()
      }
      Value::Null => f.debug_struct("Null").finish(),
      Value::Bool(b) => b.fmt(f),
      Value::Int(i) => i.fmt(f),
      Value::Float(g) => g.fmt(f),
      Value::List(l) => {
        let mut m = f.debug_list();
        for item in l.iter() {
          m.entry(&DebugRef {
            value: item,
            seen: self.seen,
          });
        }
        m.finish()
      }
      Value::Attrs(attrs) => {
        let mut f = f.debug_map();
        for (k, v) in attrs.iter() {
          f.entry(
            k,
            &DebugRef {
              value: v,
              seen: self.seen,
            },
          );
        }
        f.finish()
      }
      Value::String(StringCtx { s, .. }) => s.fmt(f),
      Value::Lambda { fun, .. } => write!(f, "lambda @ {:?}", fun.argument.span),
      Value::Primop(p) => p.fmt(f),
    }
  }
}

impl Debug for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    #[allow(clippy::mutable_key_type)]
    let seen = RefCell::new(HashSet::new());
    DebugValue {
      value: self,
      seen: &seen,
    }
    .fmt(f)
  }
}
