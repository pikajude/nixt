use super::value::ValueRef;
use crate::prelude::*;
use im::{OrdMap, Vector};
use std::sync::Arc;

pub type StaticScope = OrdMap<Ident, ValueRef>;

#[derive(Clone, Debug, Default)]
pub struct Context {
  pub scopes: Vector<Arc<Scope>>,
  pub with: Vector<ValueRef>,
}

#[derive(Debug, Clone)]
pub enum Scope {
  Dynamic(ValueRef),
  Static(StaticScope),
}

impl Context {
  pub fn new() -> Self {
    Self {
      scopes: Vector::new(),
      with: Vector::new(),
    }
  }

  pub fn single(s: Scope) -> Self {
    Self {
      scopes: Vector::from(vec![Arc::new(s)]),
      with: Vector::new(),
    }
  }

  pub fn prepend(&self, s: Scope) -> Self {
    let mut scope = self.scopes.clone();
    scope.push_front(Arc::new(s));
    Self {
      scopes: scope,
      with: self.with.clone(),
    }
  }

  pub fn add_with(&self, s: ValueRef) -> Self {
    let mut w = self.with.clone();
    w.push_front(s);
    Self {
      scopes: self.scopes.clone(),
      with: w,
    }
  }
}
