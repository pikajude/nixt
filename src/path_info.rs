use super::{hash::Hash, path::Path as StorePath};
use std::{
  collections::BTreeSet,
  fmt::Debug,
  time::{SystemTime, UNIX_EPOCH},
};

pub trait PathInfo: Send + Sync + Debug {
  fn store_path(&self) -> &StorePath;
}

#[derive(Clone, Debug)]
pub struct ValidPathInfo {
  pub store_path: StorePath,
  pub deriver: Option<StorePath>,
  pub nar_hash: Hash,
  pub references: BTreeSet<StorePath>,
  pub registration_time: SystemTime,
  pub nar_size: Option<u64>,
  pub id: u64,
  pub signatures: BTreeSet<String>,
  pub content_addressed: Option<String>,
  pub ultimate: bool,
}

impl ValidPathInfo {
  pub fn new(store_path: StorePath, nar_hash: Hash) -> Self {
    Self {
      store_path,
      nar_hash,
      deriver: Default::default(),
      references: Default::default(),
      registration_time: UNIX_EPOCH,
      nar_size: Default::default(),
      id: Default::default(),
      signatures: Default::default(),
      content_addressed: Default::default(),
      ultimate: false,
    }
  }
}

impl PartialEq for ValidPathInfo {
  fn eq(&self, other: &Self) -> bool {
    self.store_path == other.store_path
      && self.nar_hash == other.nar_hash
      && self.references == other.references
  }
}

impl Eq for ValidPathInfo {}

impl PathInfo for ValidPathInfo {
  fn store_path(&self) -> &StorePath {
    &self.store_path
  }
}
