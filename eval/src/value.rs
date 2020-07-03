use crate::scope::{Context, StaticScope};
use arena::Id;
use expr::{ExprId, ExprRef};
use std::collections::HashSet;
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

  pub fn typename(&self) -> Typename {
    match self {
      Self::Int { .. } => Typename::Int,
      Self::Float { .. } => Typename::Float,
      x => unimplemented!("{:?}", x),
    }
  }
}

#[derive(derive_more::Display, Debug)]
pub enum Typename {
  #[display(fmt = "an integer")]
  Int,
  #[display(fmt = "a float")]
  Float,
  #[display(fmt = "an unknown value")]
  Unknown,
}
