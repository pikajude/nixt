#![feature(unboxed_closures, fn_traits)]

#[macro_use] extern crate log;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate lazy_static;

pub mod archive;
pub mod base32;
pub mod derivation;
pub mod hash;
pub mod path;
pub mod path_info;
mod settings;
pub mod store;

pub use settings::Settings;
pub use store::Store;

pub fn settings() -> &'static Settings {
  Settings::get()
}
