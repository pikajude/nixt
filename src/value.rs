use crate::{
  eval::Eval,
  prelude::*,
  syntax::{
    expr::{ExprRef, Lambda},
    parse::Located,
  },
};
use std::{
  collections::{BTreeSet, HashMap},
  fmt,
  fmt::Debug,
  path::PathBuf,
};

pub type ValueRef = Writable<Value>;
pub type EnvRef = Writable<Env>;
pub type PathSet = BTreeSet<String>;
pub type Attrs = HashMap<Ident, Located<ValueRef>>;

#[derive(Derivative, EnumAsInner, Clone)]
#[derivative(Debug)]
pub enum Value {
  Null,
  Bool(bool),
  Int(i64),
  Float(f64),
  String(Str),
  Path(PathBuf),
  Attrs(Readable<Attrs>),
  List(Readable<Vec<ValueRef>>),
  Apply(ValueRef, ValueRef),
  Thunk(Thunk),
  Lambda(EnvRef, Readable<Lambda>),
  Primop(Primop, Vec<ValueRef>),
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Thunk {
  #[derivative(Debug = "ignore")]
  pub env: EnvRef,
  pub expr: ExprRef,
}

impl Clone for Thunk {
  fn clone(&self) -> Self {
    panic!("thunks may not be cloned")
  }
}

#[derive(Debug, Clone)]
pub struct Str {
  pub s: String,
  pub context: PathSet,
}

impl Value {
  pub fn typename(&self) -> &'static str {
    match self {
      Value::Null => "null",
      Value::Bool { .. } => "bool",
      Value::Int { .. } => "int",
      Value::Float { .. } => "float",
      Value::String { .. } => "string",
      Value::Path { .. } => "path",
      Value::Attrs { .. } => "attrset",
      Value::List { .. } => "list",
      Value::Lambda { .. } => "lambda",
      Value::Primop(_, args) => {
        if args.is_empty() {
          "primop"
        } else {
          "primop-app"
        }
      }
      Value::Thunk { .. } => "thunk",
      Value::Apply { .. } => "function application",
    }
  }

  pub fn thunk(env: EnvRef, expr: ExprRef) -> Self {
    Self::Thunk(Thunk { env, expr })
  }
}

#[derive(Debug)]
pub struct Env {
  pub prev_with: Option<u8>,
  pub values: Vec<ValueRef>,
  pub env_type: EnvType,
  pub up: Option<EnvRef>,
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum EnvType {
  Plain,
  HasWithExpr,
  HasWithAttrs,
}

impl Env {
  pub fn climb(e: EnvRef, l: u8) -> EnvRef {
    if l == 0 {
      e
    } else {
      Self::climb(
        e.read()
          .up
          .as_ref()
          .expect("incorrect offset in level()")
          .clone(),
        l - 1,
      )
    }
  }
}

impl Default for Env {
  fn default() -> Self {
    Self {
      up: None,
      prev_with: None,
      values: vec![],
      env_type: EnvType::HasWithAttrs,
    }
  }
}

pub trait PrimopFn = Fn(&Eval, Pos, Vec<ValueRef>) -> Result<Value>;

#[derive(Clone)]
pub struct Primop {
  pub fun: Readable<dyn PrimopFn>,
  pub name: Ident,
  pub arity: u8,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<primop {}>", self.name)
  }
}
