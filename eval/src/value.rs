use crate::{
  error::{Error, ErrorKind},
  scope::{Context, StaticScope},
  Eval, Result,
};
use arena::Id;
use expr::{ExprId, ExprRef};
use std::{
  collections::HashSet,
  fmt::{self, Debug},
  path::PathBuf,
  sync::Arc,
};
use syntax::{expr, span::Spanned};

pub type ValueRef = ValueId;
pub type ValueId = Id<Spanned<Value>>;

#[derive(Debug)]
pub enum Value {
  Int(i64),
  Float(f64),
  String {
    s: String,
    context: HashSet<String>,
  },
  Path(PathBuf),
  Lambda {
    arg: Spanned<expr::LambdaArg>,
    body: ExprRef,
    captures: Context,
  },
  Attrset(StaticScope),
  App {
    lhs: ValueRef,
    rhs: ValueRef,
  },
  Primop(Primop),
  Thunk {
    id: ExprId,
    context: Context,
  },
  Pointer(ValueId),
  Blackhole,
}

impl Value {
  pub fn is_real(&self) -> bool {
    match self {
      Self::Blackhole { .. } | Self::Pointer { .. } | Self::Thunk { .. } | Self::App { .. } => {
        false
      }
      _ => true,
    }
  }

  pub fn is_thunk(&self) -> bool {
    match self {
      Self::Thunk { .. } => true,
      _ => false,
    }
  }

  pub fn take_thunk(&mut self) -> Option<(Context, ExprId)> {
    if let Self::Thunk { .. } = self {
      if let Self::Thunk { context, id } = std::mem::replace(self, Self::Blackhole) {
        Some((context, id))
      } else {
        unreachable!()
      }
    } else {
      None
    }
  }

  pub fn typename(&self) -> Typename {
    match self {
      Self::Int { .. } => Typename::Int,
      Self::Float { .. } => Typename::Float,
      Self::Attrset { .. } => Typename::Attrset,
      Self::Primop(p) => Typename::Primop(p.name()),
      x => unimplemented!("{:?}", x),
    }
  }

  pub fn expected(&self, expected: Typename) -> Error {
    ErrorKind::TypeMismatch {
      actual: self.typename(),
      expected,
    }
    .into()
  }
}

#[derive(derive_more::Display, Debug)]
pub enum Typename {
  #[display(fmt = "an integer")]
  Int,
  #[display(fmt = "a float")]
  Float,
  #[display(fmt = "an attrset")]
  Attrset,
  #[display(fmt = "the primop {:?}", _0)]
  Primop(&'static str),
  #[display(fmt = "an unknown value")]
  Unknown,
}

macro_rules! primop_types {
  ($($t:tt)+) => {
    type DynOp = Box<dyn Fn $($t)+>;
    type StaticOp = fn $($t)+;
  }
}

primop_types! {
  (&mut Eval, ValueRef) -> Result<Value>
}

#[derive(Clone)]
pub enum Primop {
  Dynamic { name: &'static str, op: Arc<DynOp> },
  Static { name: &'static str, op: StaticOp },
}

impl Primop {
  pub fn name(&self) -> &'static str {
    match self {
      Self::Dynamic { name, .. } | Self::Static { name, .. } => name,
    }
  }

  pub fn static_(name: &'static str, op: StaticOp) -> Self {
    Self::Static { name, op }
  }

  pub fn dynamic<F: Fn(&mut Eval, ValueRef) -> Result<Value> + 'static>(
    name: &'static str,
    cb: F,
  ) -> Self {
    Self::Dynamic {
      name,
      op: Arc::new(Box::new(cb)),
    }
  }

  pub fn call(&self, e: &mut Eval, id: ValueRef) -> Result<Value> {
    match self {
      Self::Dynamic { op, .. } => op(e, id),
      Self::Static { op, .. } => op(e, id),
    }
  }

  pub fn primop2(
    name: &'static str,
    op: fn(&mut Eval, ValueRef, ValueRef) -> Result<Value>,
  ) -> Self {
    Self::Dynamic {
      name,
      op: Arc::new(Box::new(move |_, node1| {
        Ok(Value::Primop(Self::dynamic(
          "primop-app",
          move |eval, node2| op(eval, node1, node2),
        )))
      })),
    }
  }
}

impl Debug for Primop {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<primop {:?}>", self.name())
  }
}
