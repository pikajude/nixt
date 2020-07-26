#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(untagged_unions)]
#![feature(btree_drain_filter)]

#[macro_use] extern crate log;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate lazy_static;

pub mod archive;
mod arena;
pub mod base32;
pub mod derivation;
pub mod eval;
pub mod hash;
pub mod path;
pub mod path_info;
pub mod settings;
pub mod store;
pub mod syntax;
pub mod util;

pub use settings::Settings;
pub use store::Store;

pub fn settings() -> &'static Settings {
  Settings::get()
}
