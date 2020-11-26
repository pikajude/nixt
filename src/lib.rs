#![feature(map_first_last)]
#![feature(min_const_generics)]
#![feature(trait_alias)]
#![feature(btree_drain_filter)]

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

// #[cfg(not(feature = "lock-free"))]
pub mod lock {
  pub use parking_lot::RwLock;
}

// #[cfg(feature = "lock-free")] pub mod lock;

pub mod error;
pub mod eval;
pub mod logger;
pub mod prelude;
pub mod syntax;
pub mod value;
