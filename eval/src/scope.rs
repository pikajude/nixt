use crate::value::ValueRef;
use cons_list::ConsList;
use std::collections::HashMap;
use syntax::expr::Ident;

#[derive(Debug)]
pub enum Scope {
  Dynamic(ValueRef),
  Static(StaticScope),
}

pub type StaticScope = HashMap<Ident, ValueRef>;
pub type Context = ConsList<Scope>;
