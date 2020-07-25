use super::thunk::ThunkId;
use crate::{
  context::{Context, StaticScope},
  primop::Primop,
};
use nix_syntax::expr;
use std::{collections::BTreeSet, path::PathBuf};

// You'd think this was a set of filepaths, but it's actually a set of formatted
// strings that contain paths
pub type PathSet = BTreeSet<String>;

#[derive(Debug)]
pub enum Value {
  Null,
  Int(i64),
  Float(f64),
  Bool(bool),
  String {
    string: String,
    context: PathSet,
  },
  Path(PathBuf),
  Lambda {
    lambda: expr::Lambda,
    captures: Box<Context>,
  },
  Primop(Primop),
  AttrSet(StaticScope),
  List(Vec<ThunkId>),
  Ref(ThunkId),
}

impl Value {
  pub fn typename(&self) -> &'static str {
    match self {
      Value::Null => "null",
      Value::Int(_) => "int",
      Value::Float(_) => "float",
      Value::Bool(_) => "bool",
      Value::String { .. } => "string",
      Value::Path(_) => "path",
      Value::Lambda { .. } => "lambda",
      Value::Primop(_) => "primop",
      Value::AttrSet(_) => "attrset",
      Value::List(_) => "list",
      Value::Ref(_) => panic!(),
    }
  }

  pub fn string_bare<S: Into<String>>(string: S) -> Self {
    Value::String {
      string: string.into(),
      context: Default::default(),
    }
  }
}
