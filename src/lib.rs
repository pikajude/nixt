#![feature(btree_drain_filter)]
#![feature(doc_cfg)]
#![feature(duration_zero)]
#![feature(fn_traits)]
#![feature(pattern)]
#![feature(unboxed_closures)]
#![feature(untagged_unions)]

#[macro_use] extern crate log;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate gen_settings;

pub mod archive;
pub mod arena;
pub mod derivation;
pub mod eval;
pub mod fetch;
pub mod goal;
pub mod hash;
pub mod path;
pub mod path_info;
mod prelude;
pub mod settings;
pub mod sqlite;
pub mod store;
pub mod sync;
pub mod syntax;
pub mod util;

pub use settings::Settings;
pub use store::Store;

pub fn settings() -> &'static Settings {
  Settings::get()
}
