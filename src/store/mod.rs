use crate::{archive::PathFilter, prelude::*};
use std::{
  collections::{BTreeMap, BTreeSet},
  ffi::OsStr,
  fmt::Display,
  sync::Arc,
};

mod local;

pub use local::*;

pub(crate) fn show_path<'a>(i: &'a OsStr) -> impl Display + 'a {
  Path::new(i).display()
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum RepairFlag {
  NoRepair,
  Repair,
}

impl RepairFlag {
  pub fn repair(self) -> bool {
    self == Self::Repair
  }
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum CheckSigsFlag {
  NoCheckSigs,
  CheckSigs,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum FileIngestionMethod {
  Flat,
  Recursive,
}

pub trait Store: Send + Sync {
  fn store_path(&self) -> Cow<OsStr>;

  fn print_store_path(&self, path: &StorePath) -> String {
    format!("{}/{}", show_path(&self.store_path()), path)
  }

  fn make_store_path(&self, path_type: &str, hash: &Hash, name: &str) -> Result<StorePath> {
    let ident = format!(
      "{}:{}:{}:{}",
      path_type,
      hash.encode(Encoding::Base16),
      show_path(&self.store_path()),
      name
    );
    let hash = Hash::hash_bytes(ident.as_bytes(), HashType::SHA256)
      .truncate(20)
      .into_owned();
    StorePath::from_parts(hash.as_bytes(), name)
  }

  fn parse_store_path(&self, path: &Path) -> Result<StorePath> {
    StorePath::new(path, self.store_path().as_ref())
  }

  fn parse_path_with_outputs(&self, input: &str) -> Result<StorePathWithOutputs> {
    let (path, outputs) = if let Some(n) = input.find('!') {
      (
        &input[..n],
        input[n + 1..].split(',').map(String::from).collect(),
      )
    } else {
      (input, Default::default())
    };

    Ok(StorePathWithOutputs {
      path: StorePath::new(Path::new(path), self.store_path().as_ref())?,
      outputs,
    })
  }

  fn parse_derivation(&self, path: &Path, name: &str) -> Result<Derivation> {
    Derivation::parse(self, &fs::read_to_string(path)?, name)
  }

  fn read_derivation(&self, path: &StorePath) -> Result<Derivation> {
    self.parse_derivation(
      &self.to_real_path(path)?,
      &Derivation::name_from_path(path)?,
    )
  }

  fn to_real_path(&self, path: &StorePath) -> Result<PathBuf> {
    Ok(self.print_store_path(path).into())
  }

  fn get_path_info(&self, path: &StorePath) -> Result<Option<Rc<dyn PathInfo>>>;

  fn is_valid_path(&self, path: &StorePath) -> Result<bool> {
    self.get_path_info(path).map(|x| x.is_some())
  }

  fn is_in_store(&self, path: &Path) -> bool {
    path.starts_with(self.store_path())
  }

  fn register_valid_path(&self, path_info: ValidPathInfo) -> Result<()>;

  fn add_temp_root(&self, _path: &StorePath) -> Result<()> {
    bail!("not supported by this store backend")
  }

  fn make_type<'a>(
    &self,
    mut s: String,
    references: &mut dyn Iterator<Item = &'a StorePath>,
    has_self_reference: bool,
  ) -> String {
    for item in references {
      s.push(':');
      s.push_str(&self.print_store_path(item));
    }
    if has_self_reference {
      s.push_str(":self");
    }
    s
  }

  fn make_output_path(&self, id: &str, hash: &Hash, name: &str) -> Result<StorePath> {
    self.make_store_path(
      &format!("output:{}", id),
      hash,
      &format!(
        "{}{}{}",
        name,
        if id == "out" { "" } else { "-" },
        if id == "out" { "" } else { id }
      ),
    )
  }

  fn make_fixed_output_path<'a>(
    &self,
    ingest_method: FileIngestionMethod,
    hash: &Hash,
    name: &str,
    references: &mut dyn Iterator<Item = &'a StorePath>,
    has_self_reference: bool,
  ) -> Result<StorePath> {
    let recursive = ingest_method == FileIngestionMethod::Recursive;
    if hash.type_() == HashType::SHA256 && recursive {
      self.make_store_path(
        &self.make_type("source".into(), references, has_self_reference),
        hash,
        name,
      )
    } else {
      assert!(references.next().is_none());
      self.make_store_path(
        "output:out",
        &Hash::hash_str(
          &format!(
            "fixed:out:{}{}:",
            if recursive { "r:" } else { "" },
            hash.encode(Encoding::Base16)
          ),
          HashType::SHA256,
        ),
        name,
      )
    }
  }

  fn make_text_path<'a>(
    &self,
    name: &str,
    hash: &Hash,
    references: &mut dyn Iterator<Item = &'a StorePath>,
  ) -> Result<StorePath> {
    assert!(hash.type_() == HashType::SHA256);
    self.make_store_path(
      &self.make_type("text".into(), references, false),
      hash,
      name,
    )
  }

  fn store_path_for_text<'a>(
    &self,
    name: &str,
    contents: &str,
    references: &mut dyn Iterator<Item = &'a StorePath>,
  ) -> Result<StorePath> {
    self.make_text_path(
      name,
      &Hash::hash_str(contents, HashType::SHA256),
      references,
    )
  }

  fn hash_derivation_modulo(&self, derivation: &Derivation, mask_outputs: bool) -> Result<Hash> {
    if derivation.is_fixed_output() {
      let out = &derivation.outputs["out"];
      let hash = out.hash.as_ref().unwrap();
      return Ok(Hash::hash_str(
        &format!(
          "fixed:out:{}:{}:{}",
          hash.method_algo(),
          hash.hash.encode(Encoding::Base16),
          self.print_store_path(&out.path)
        ),
        HashType::SHA256,
      ));
    }

    let mut inputs2: BTreeMap<String, &BTreeSet<String>> = Default::default();

    for (k, v) in &derivation.input_derivations {
      if let Some(known) = super::derivation::DRV_HASHES.lookup(k) {
        inputs2.insert(known.encode(Encoding::Base16), v);
      } else {
        let sub_hash = self.hash_derivation_modulo(&self.read_derivation(k)?, false)?;
        inputs2.insert(sub_hash.encode(Encoding::Base16), v);
        super::derivation::DRV_HASHES.add(k.clone(), sub_hash);
      }
    }

    Ok(Hash::hash_str(
      &derivation.unparse(self, mask_outputs, inputs2),
      HashType::SHA256,
    ))
  }

  fn write_derivation(
    &self,
    derivation: &Derivation,
    name: &str,
    repair: RepairFlag,
  ) -> Result<StorePath> {
    let mut refs = derivation.input_sources.iter().collect::<BTreeSet<_>>();
    for k in derivation.input_derivations.keys() {
      refs.insert(k);
    }
    let suffix = format!("{}.drv", name);
    let contents = derivation.unparse(self, false, Default::default());
    let refs = &mut refs.into_iter();
    if settings().read_only {
      self.store_path_for_text(&suffix, &contents, refs)
    } else {
      self.add_text_to_store(&suffix, &contents, refs, repair)
    }
  }

  fn add_text_to_store<'a>(
    &self,
    _name: &str,
    _contents: &str,
    _references: &mut dyn Iterator<Item = &'a StorePath>,
    _repair: RepairFlag,
  ) -> Result<StorePath> {
    bail!("add_text_to_store not supported by this backend.")
  }

  fn add_to_store_from_source(
    &self,
    info: &dyn PathInfo,
    source: &mut dyn std::io::Read,
    repair: RepairFlag,
    check_signatures: CheckSigsFlag,
  ) -> Result<()>;

  fn add_to_store_from_path(
    &self,
    name: &str,
    path: &Path,
    ingest_method: FileIngestionMethod,
    hash_type: HashType,
    path_filter: &PathFilter,
    repair: RepairFlag,
  ) -> Result<StorePath>;

  fn build_paths(self: Arc<Self>, _paths: Vec<StorePathWithOutputs>) -> Result<()> {
    bail!(
      "store backend {} does not support building paths",
      self.store_path().to_string_lossy()
    )
  }

  fn compute_closure(
    &self,
    path: &StorePath,
    closure: &mut BTreeSet<StorePath>,
    backwards: bool,
    include_outputs: bool,
    include_derivers: bool,
  ) -> Result<()>;
}
