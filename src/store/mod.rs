use crate::{archive::PathFilter, prelude::*};
use std::{
  borrow::Borrow,
  collections::{BTreeMap, BTreeSet},
  ffi::OsStr,
  fmt::{Debug, Display},
};

mod local;

pub use local::*;

#[allow(clippy::needless_lifetimes)] // clippy pls
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

#[derive(Copy, Clone, Debug, Default)]
pub struct ClosureOpts {
  backwards: bool,
  include_outputs: bool,
  include_derivers: bool,
}

pub trait Store: Send + Sync + Debug {
  fn store_path(&self) -> Cow<OsStr>;

  fn print_store_path<P: Borrow<StorePath>>(&self, path: P) -> String {
    format!("{}/{}", show_path(&self.store_path()), path.borrow())
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

  fn parse_store_path<P: AsRef<Path>>(&self, path: P) -> Result<StorePath> {
    StorePath::new(path.as_ref(), self.store_path().as_ref())
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

  fn read_derivation<P: Borrow<StorePath>>(&self, path: P) -> Result<Derivation> {
    Derivation::get(self, path)
  }

  fn to_real_path<P: Borrow<StorePath>>(&self, path: P) -> Result<PathBuf> {
    Ok(self.print_store_path(path).into())
  }

  fn get_path_info<P: Borrow<StorePath>>(&self, path: P) -> Result<Option<Rc<dyn PathInfo>>>;

  fn is_valid_path(&self, path: &StorePath) -> Result<bool> {
    self.get_path_info(path).map(|x| x.is_some())
  }

  fn is_in_store(&self, path: &Path) -> bool {
    path.starts_with(self.store_path())
  }

  fn register_valid_paths<I: IntoIterator<Item = ValidPathInfo>>(
    &self,
    path_infos: I,
  ) -> Result<()>;

  fn register_valid_path(&self, path_info: ValidPathInfo) -> Result<()> {
    self.register_valid_paths(std::iter::once(path_info))
  }

  fn add_temp_root(&self, _path: &StorePath) -> Result<()> {
    bail!("not supported by this store backend")
  }

  fn make_type<I: IntoIterator<Item = StorePath>>(
    &self,
    mut s: String,
    references: I,
    has_self_reference: bool,
  ) -> String {
    for item in references {
      s.push(':');
      s.push_str(&self.print_store_path(&item));
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

  fn make_fixed_output_path<I: IntoIterator<Item = StorePath>>(
    &self,
    ingest_method: FileIngestionMethod,
    hash: &Hash,
    name: &str,
    references: I,
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
      assert!(references.into_iter().next().is_none());
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

  fn make_text_path<I: IntoIterator<Item = StorePath>>(
    &self,
    name: &str,
    hash: &Hash,
    references: I,
  ) -> Result<StorePath> {
    assert!(hash.type_() == HashType::SHA256);
    self.make_store_path(
      &self.make_type("text".into(), references, false),
      hash,
      name,
    )
  }

  fn store_path_for_text<I: IntoIterator<Item = StorePath>>(
    &self,
    name: &str,
    contents: &str,
    references: I,
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
    let mut refs = derivation
      .input_sources
      .iter()
      .cloned()
      .collect::<BTreeSet<_>>();
    for k in derivation.input_derivations.keys() {
      refs.insert(k.clone());
    }
    let suffix = format!("{}.drv", name);
    let contents = derivation.unparse(self, false, Default::default());
    if settings().read_only {
      self.store_path_for_text(&suffix, &contents, refs)
    } else {
      self.add_text_to_store(&suffix, &contents, refs, repair)
    }
  }

  #[allow(unused_variables)] // we want nice variable names for rustdoc
  fn add_text_to_store<I: IntoIterator<Item = StorePath>>(
    &self,
    name: &str,
    contents: &str,
    references: I,
    repair: RepairFlag,
  ) -> Result<StorePath> {
    bail!("add_text_to_store not supported by this backend.")
  }

  fn add_to_store_from_source<I: PathInfo, R: std::io::Read>(
    &self,
    info: I,
    source: R,
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

  #[allow(unused_variables)]
  fn build_paths(&self, paths: Vec<StorePathWithOutputs>) -> Result<()> {
    bail!(
      "store backend {} does not support building paths",
      self.store_path().to_string_lossy()
    )
  }

  fn compute_closure(
    &self,
    path: &StorePath,
    closure: &mut BTreeSet<StorePath>,
    options: ClosureOpts,
  ) -> Result<()>;

  fn logfile_of(&self, path: &StorePath) -> PathBuf {
    let mut log_part0 = path.to_string();
    let log_part1 = log_part0.split_off(2);

    settings()
      .paths
      .nix_log_dir
      .join("drvs")
      .join(log_part0)
      .join(log_part1)
  }
}
