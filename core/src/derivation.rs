use crate::{hash::Hash, path::Path as StorePath};
use async_std::{path::PathBuf, sync::Mutex};
use std::collections::{HashMap, HashSet};

lazy_static! {
  pub static ref DRV_HASHES: Mutex<HashMap<StorePath, Hash>> = Mutex::new(HashMap::new());
}

#[derive(Debug, Eq, PartialEq)]
pub struct FixedOutputHash {
  pub recursive: bool,
  pub hash: Hash,
}

impl FixedOutputHash {
  pub fn method_algo(&self) -> String {
    format!(
      "{}{}",
      if self.recursive { "r:" } else { "" },
      self.hash.type_()
    )
  }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Output {
  pub path: StorePath,
  pub hash: Option<FixedOutputHash>,
}

#[derive(Default, Debug)]
pub struct Derivation {
  pub name: String,
  pub builder: Option<PathBuf>,
  pub system: Option<String>,
  pub args: Vec<String>,
  pub env: HashMap<String, String>,
  pub input_sources: HashSet<StorePath>,
  pub outputs: HashMap<String, Output>,
  pub input_derivations: HashMap<StorePath, HashSet<String>>,
}

impl Derivation {
  pub fn is_fixed_output(&self) -> bool {
    self.outputs.len() == 1 && self.outputs.get("out").map_or(false, |x| x.hash.is_some())
  }
}
