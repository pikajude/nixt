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

#[derive(Debug, EnumAsInner, Clone)]
pub enum Value {
  Null,
  Bool(bool),
  Int(i64),
  Float(f64),
  String(Str),
  Path(PathBuf),
  Attrs(Writable<HashMap<Ident, Located<ValueRef>>>),
  List1(ValueRef),
  List2(ValueRef, ValueRef),
  List(Readable<Vec<ValueRef>>),
  Apply(ValueRef, ValueRef),
  Thunk(EnvRef, ExprRef),
  Lambda(EnvRef, Readable<Lambda>),
  Primop(Primop, Vec<ValueRef>),
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
      Value::List1 { .. } | Value::List2 { .. } | Value::List { .. } => "list",
      Value::Lambda { .. } => "lambda",
      Value::Primop { .. } => "primop",
      Value::Thunk { .. } => "thunk",
      Value::Apply { .. } => "function application",
    }
  }
}

#[derive(Debug)]
pub struct Env {
  pub up: Option<EnvRef>,
  pub prev_with: Option<u8>,
  pub values: Vec<ValueRef>,
  pub env_type: EnvType,
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
