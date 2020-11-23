#![feature(map_first_last)]
#![feature(trait_alias)]

#[macro_use] extern crate slog_scope;
#[macro_use] extern crate derivative;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate enum_as_inner;
#[macro_use] extern crate thiserror;
#[macro_use] extern crate lazy_static;

#[macro_use]
mod atoms {
  #![allow(unused)]
  include!(concat!(env!("OUT_DIR"), "/ident.rs"));
}

pub mod error;
pub mod eval;
pub mod logger;
pub mod prelude;
pub mod syntax;
pub mod value;
