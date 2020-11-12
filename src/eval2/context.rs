use super::value::ValueRef;
use crate::prelude::*;
use std::collections::HashMap;

pub type StaticScope = HashMap<Ident, ValueRef>;

#[derive(Clone, Debug, Default)]
pub struct Context {
  pub scopes: ConsList<Scope>,
  pub with: ConsList<ValueRef>,
}

#[derive(Debug, Clone)]
pub enum Scope {
  Dynamic(ValueRef),
  Static(StaticScope),
}

impl Context {
  pub fn single(s: Scope) -> Self {
    Self {
      scopes: std::iter::once(s).collect(),
      with: Default::default(),
    }
  }

  pub fn prepend(&self, s: Scope) -> Self {
    Self {
      scopes: self.scopes.cons(s),
      with: self.with.clone(),
    }
  }

  pub fn add_with(&self, s: ValueRef) -> Self {
    Self {
      scopes: self.scopes.clone(),
      with: self.with.cons(s),
    }
  }
}
