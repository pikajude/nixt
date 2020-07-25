use crate::{
  derivation::Derivation,
  hash::{Encoding, Hash, HashType},
  path::Path as StorePath,
};
use nix_util::*;
use std::{
  borrow::Cow,
  collections::{BTreeMap, BTreeSet},
  ffi::OsStr,
  fmt::Display,
  path::Path as StdPath,
};

pub(crate) fn show_path<'a>(i: &'a OsStr) -> impl Display + 'a {
  StdPath::new(i).display()
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

  fn make_type<'a>(
    &self,
    mut s: String,
    references: Vec<&'a StorePath>,
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
    recursive: bool,
    hash: &Hash,
    name: &str,
    references: Vec<&'a StorePath>,
    has_self_reference: bool,
  ) -> Result<StorePath> {
    if hash.type_() == HashType::SHA256 && recursive {
      self.make_store_path(
        &self.make_type("source".into(), references, has_self_reference),
        hash,
        name,
      )
    } else {
      assert!(references.is_empty());
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
      let mut hashes = crate::derivation::DRV_HASHES.lock().unwrap();
      if let Some(known) = hashes.get(k) {
        inputs2.insert(known.encode(Encoding::Base16), v);
      } else {
        let sub_hash = self.hash_derivation_modulo(derivation, false)?;
        inputs2.insert(sub_hash.encode(Encoding::Base16), v);
        hashes.insert(k.clone(), sub_hash);
      }
    }

    Ok(Hash::hash_str(
      &derivation.unparse(self, mask_outputs, inputs2),
      HashType::SHA256,
    ))
  }
}

pub struct LocalStore;

impl Store for LocalStore {
  fn store_path(&self) -> Cow<OsStr> {
    Cow::Borrowed(OsStr::new("/nix/store"))
  }
}
