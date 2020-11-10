use super::value::ValueRef;
use crate::prelude::*;
use std::{collections::BTreeMap, sync::Arc};

pub type StaticScope = BTreeMap<Ident, ValueRef>;

#[derive(Clone, Debug, Default)]
pub struct Context {
  pub scopes: Vec<Arc<Scope>>,
  pub with: Vec<ValueRef>,
}

#[derive(Debug, Clone)]
pub enum Scope {
  Dynamic(ValueRef),
  Static(StaticScope),
}

impl Context {
  pub fn single(s: Scope) -> Self {
    Self {
      scopes: vec![Arc::new(s)],
      with: vec![],
    }
  }

  pub fn prepend(&self, s: Scope) -> Self {
    let mut scope = self.scopes.clone();
    scope.insert(0, Arc::new(s));
    Self {
      scopes: scope,
      with: self.with.clone(),
    }
  }

  pub fn add_with(&self, s: ValueRef) -> Self {
    let mut w = self.with.clone();
    w.insert(0, s);
    Self {
      scopes: self.scopes.clone(),
      with: w,
    }
  }
}
