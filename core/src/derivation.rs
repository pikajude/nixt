use crate::{
  hash::{Encoding, Hash, HashType},
  path::Path as StorePath,
  Store,
};
use std::{
  collections::{BTreeMap, BTreeSet, HashMap},
  fmt::{Display, Write},
  path::PathBuf,
  sync::Mutex,
};

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
  pub builder: PathBuf,
  pub platform: String,
  pub args: Vec<String>,
  pub env: BTreeMap<String, String>,
  pub input_sources: BTreeSet<StorePath>,
  pub outputs: BTreeMap<String, Output>,
  pub input_derivations: BTreeMap<StorePath, BTreeSet<String>>,
}

macro_rules! unquoted {
  ($x:expr, $y:expr) => {
    unquoted!($x, "{}", $y)
  };

  ($x:expr, $y:literal, $($t:tt)+) => {
    write!($x, concat!("\"", $y, "\""), $($t)+).unwrap()
  }
}

impl Derivation {
  pub fn is_fixed_output(&self) -> bool {
    self.outputs.len() == 1 && self.outputs.get("out").map_or(false, |x| x.hash.is_some())
  }

  pub fn unparse<S: Store + ?Sized>(
    &self,
    store: &S,
    mask_outputs: bool,
    actual_inputs: BTreeMap<String, &BTreeSet<String>>,
  ) -> String {
    let mut s = String::with_capacity(65536);
    s.push_str("Derive([");

    for (i, (out_name, out)) in self.outputs.iter().enumerate() {
      if i > 0 {
        s.push(',');
      }
      s.push('(');
      unquoted!(s, out_name);
      s.push(',');
      if mask_outputs {
        unquoted!(s, "");
      } else {
        unquoted!(s, store.print_store_path(&out.path));
      }

      s.push(',');
      if let Some(h) = out.hash.as_ref() {
        unquoted!(s, &h.method_algo());
        s.push(',');
        unquoted!(s, &h.hash.encode(Encoding::Base16));
      } else {
        unquoted!(s, "");
        s.push(',');
        unquoted!(s, "");
      }
      s.push(')');
    }

    s.push_str("],[");

    if !actual_inputs.is_empty() {
      for (i, (input_name, input_value)) in actual_inputs.iter().enumerate() {
        if i > 0 {
          s.push(',');
        }
        s.push('(');
        unquoted!(s, input_name);
        s.push(',');
        print_unquoted_strings(&mut s, *input_value);
        s.push(')');
      }
    } else {
      for (i, (input_name, input_value)) in self.input_derivations.iter().enumerate() {
        if i > 0 {
          s.push(',');
        }
        s.push('(');
        unquoted!(s, input_name);
        s.push(',');
        print_unquoted_strings(&mut s, input_value);
        s.push(')');
      }
    }

    s.push_str("],");

    print_unquoted_strings(
      &mut s,
      self.input_sources.iter().map(|i| store.print_store_path(i)),
    );

    s.push(',');
    unquoted!(s, &self.platform);
    s.push(',');
    print_string(&mut s, self.builder.display().to_string().as_str());
    s.push(',');
    print_strings(&mut s, self.args.iter().map(|x| x.as_str()));

    s.push_str(",[");

    for (i, (k, v)) in self.env.iter().enumerate() {
      if i > 0 {
        s.push(',');
      }
      s.push('(');
      print_string(&mut s, k);
      s.push(',');
      if mask_outputs && self.outputs.contains_key(k) {
        print_string(&mut s, "");
      } else {
        print_string(&mut s, v);
      }
      s.push(')');
    }

    s.push_str("])");

    s
  }
}

fn print_unquoted_strings<I: IntoIterator<Item = D>, D: Display>(s: &mut String, items: I) {
  s.push('[');
  for (i, item) in items.into_iter().enumerate() {
    if i > 0 {
      s.push(',');
    }
    unquoted!(s, item);
  }
  s.push(']');
}

fn print_string(s: &mut String, input: &str) {
  s.push('"');
  for c in input.chars() {
    match c {
      '"' | '\\' => {
        s.push('\\');
        s.push(c);
      }
      '\n' => s.push_str("\\n"),
      '\r' => s.push_str("\\r"),
      '\t' => s.push_str("\\t"),
      x => s.push(x),
    }
  }
  s.push('"');
}

fn print_strings<'a, I: IntoIterator<Item = &'a str>>(s: &mut String, strs: I) {
  s.push('[');
  for (i, item) in strs.into_iter().enumerate() {
    if i > 0 {
      s.push(',');
    }
    print_string(s, item);
  }
  s.push(']');
}

pub fn hash_placeholder<S: AsRef<str>>(output: S) -> String {
  format!(
    "/{}",
    Hash::hash_str(&format!("nix-output:{}", output.as_ref()), HashType::SHA256)
      .encode(Encoding::Base32)
  )
}
