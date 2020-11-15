use crate::{
  prelude::*,
  syntax::{
    expr::{ExprRef, Lambda},
    parse::Located,
  },
};
use std::{
  collections::{BTreeSet, HashMap},
  path::PathBuf,
};

pub type ValueRef = Writable<Value>;
pub type EnvRef = Writable<Env>;

#[derive(Debug, EnumAsInner)]
pub enum Value {
  Bool(bool),
  Int(i64),
  String(String, BTreeSet<String>),
  Path(PathBuf),
  Attrs(Writable<HashMap<Ident, Located<ValueRef>>>),
  List1(ValueRef),
  List2(ValueRef, ValueRef),
  List(Readable<Vec<ValueRef>>),
  Apply(ValueRef, ValueRef),
  Thunk(EnvRef, ExprRef),
  Lambda(EnvRef, Lambda),
}

#[derive(Debug)]
pub struct Env {
  pub up: Option<EnvRef>,
  pub prev_with: u8,
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
      prev_with: 14,
      values: vec![],
      env_type: EnvType::HasWithAttrs,
    }
  }
}
