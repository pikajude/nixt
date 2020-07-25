#[macro_use] extern crate log;
#[macro_use] extern crate derive_more;
#[macro_use] extern crate lazy_static;

pub mod base32;
pub mod derivation;
pub mod hash;
pub mod path;
mod settings;
pub mod store;

pub use settings::Settings;
pub use store::Store;

pub fn settings() -> &'static Settings {
  Settings::get()
}
