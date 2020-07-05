use crate::{
  error::{Error, ErrorKind},
  scope::{Context, StaticScope},
  Eval, Result,
};
use arena::Id;
use expr::{ExprId, ExprRef};
use futures::future::BoxFuture;
use std::{
  collections::HashSet,
  fmt::{self, Debug},
  path::PathBuf,
  sync::Arc,
  thread::{self, ThreadId},
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
  List(Vec<ValueRef>),
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
  Blackhole(ThreadId),
}

static_assertions::assert_impl_all!(Value: Send);

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

  pub fn blackhole() -> Self {
    Self::Blackhole(thread::current().id())
  }

  pub fn typename(&self) -> Typename {
    match self {
      Self::Int { .. } => Typename::Int,
      Self::Float { .. } => Typename::Float,
      Self::Attrset { .. } => Typename::Attrset,
      Self::Primop(p) => Typename::Primop(p.name()),
      Self::String { .. } => Typename::String,
      Self::Path { .. } => Typename::Path,
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
  #[display(fmt = "a list")]
  List,
  #[display(fmt = "a string")]
  String,
  #[display(fmt = "a path")]
  Path,
  /* #[display(fmt = "an unknown value")]
   * Unknown, */
}

type StaticOp = for<'a> fn(&'a Eval, ValueRef) -> BoxFuture<'a, Result<Value>>;

type AsyncOp =
  Arc<Box<dyn for<'a> Fn(&'a Eval, ValueRef) -> BoxFuture<'a, Result<Value>> + Send + Sync>>;

pub enum Primop {
  Async { name: &'static str, op: AsyncOp },
  Static { name: &'static str, op: StaticOp },
}

static_assertions::assert_impl_all!(Primop: Send);

impl Primop {
  pub fn name(&self) -> &'static str {
    match self {
      Self::Static { name, .. } | Self::Async { name, .. } => name,
    }
  }

  pub fn static_(name: &'static str, op: StaticOp) -> Self {
    Self::Static { name, op }
  }

  pub fn primop2(
    name: &'static str,
    op: for<'a> fn(&'a Eval, ValueRef, ValueRef) -> BoxFuture<'a, Result<Value>>,
  ) -> Self {
    Self::Async {
      name,
      op: Arc::new(Box::new(move |_, node1| {
        Box::pin(async move {
          Ok(Value::Primop(Self::Async {
            name: "primop-app",
            op: Arc::new(Box::new(move |eval, node2| {
              Box::pin(op(eval, node1, node2))
            })),
          }))
        })
      })),
    }
  }
}

impl Debug for Primop {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<primop {:?}>", self.name())
  }
}
