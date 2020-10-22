use crate::prelude::*;
use parking_lot::Mutex;
use std::{
  collections::{BTreeMap, BTreeSet, HashMap},
  fmt::{Display, Write},
};

mod parse;
mod print;

lazy_static! {
  pub static ref DRV_HASHES: Mutex<HashMap<StorePath, Hash>> = Mutex::new(HashMap::new());
  pub static ref DERIVATIONS: Mutex<HashMap<PathHash, Derivation>> = Mutex::new(HashMap::new());
}

impl DRV_HASHES {
  pub fn lookup(&self, path: &StorePath) -> Option<Hash> {
    self.lock().get(path).cloned()
  }

  pub fn add(&self, path: StorePath, hash: Hash) -> Option<Hash> {
    self.lock().insert(path, hash)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Output {
  pub path: StorePath,
  pub hash: Option<FixedOutputHash>,
}

#[derive(Default, Debug, Clone)]
pub struct Derivation {
  pub name: String,
  pub builder: PathBuf,
  pub platform: String,
  pub args: Vec<String>,
  pub env: BTreeMap<String, String>,
  pub input_sources: BTreeSet<StorePath>,
  pub outputs: BTreeMap<String, Output>,
  pub input_derivations: BTreeMap<StorePath, BTreeSet<String>>,
}

impl Derivation {
  pub fn is_fixed_output(&self) -> bool {
    self.outputs.len() == 1 && self.outputs.get("out").map_or(false, |x| x.hash.is_some())
  }

  pub fn get_env(&self, key: &str) -> Result<&str> {
    self
      .env
      .get(key)
      .ok_or_else(|| anyhow!("attribute `{}' missing", key))
      .map(|x| &**x)
  }

  pub fn name_from_path(path: &StorePath) -> Result<String> {
    path
      .name
      .to_string()
      .strip_suffix(".drv")
      .ok_or_else(|| anyhow!("input path {} does not refer to a derivation", path))
      .map(|x| x.to_string())
  }

  pub fn out_paths(&self) -> impl Iterator<Item = &StorePath> {
    self.outputs.values().map(|v| &v.path)
  }

  pub fn is_builtin(&self) -> bool {
    self.builder.to_string_lossy().starts_with("builtin:")
  }

  pub fn can_build_locally(&self) -> bool {
    if self.platform != settings().this_system
      && !settings().extra_platforms.contains(&self.platform)
      && !self.is_builtin()
    {
      return false;
    }

    self
      .required_system_features()
      .into_iter()
      .all(|x| settings().system_features.contains(x))
  }

  pub fn required_system_features(&self) -> BTreeSet<&str> {
    self
      .env
      .get("requiredSystemFeatures")
      .map_or(Default::default(), |x| x.split_ascii_whitespace().collect())
  }

  pub fn get<S: Store + ?Sized>(store: &S, path: &StorePath) -> Result<Self> {
    let mut drv_lock = DERIVATIONS.try_lock().unwrap();
    if let Some(x) = drv_lock.get(&path.hash) {
      Ok(x.clone())
    } else {
      let drv = Self::parse(
        store,
        &fs::read_to_string(store.to_real_path(path)?)?,
        &Self::name_from_path(path)?,
      )?;
      drv_lock.insert(path.hash, drv.clone());
      Ok(drv)
    }
  }
}

pub fn hash_placeholder<S: AsRef<str>>(output: S) -> String {
  format!(
    "/{}",
    Hash::hash_str(&format!("nix-output:{}", output.as_ref()), HashType::SHA256)
      .encode(Encoding::Base32)
  )
}
