#![feature(untagged_unions)]

#[macro_use] extern crate log;
#[macro_use] extern crate async_recursion;
#[macro_use] extern crate futures;

use arena::Arena;
use async_std::path::{Path, PathBuf};
use codespan::Files;
use futures::lock::Mutex;
use std::{
  collections::{HashMap, HashSet},
  sync::{
    atomic::{AtomicU16, Ordering},
    Arc,
  },
};
use syntax::{
  expr::{self, *},
  span::{spanned, FileSpan, Spanned},
};

mod base32;
mod config;
mod error;
mod eval;
mod ext;
mod hash;
mod util;

pub use eval::Eval;

pub use config::Config;
pub use error::Result;
