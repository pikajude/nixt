use super::strings::coerce_to_string;
use crate::{
  bail,
  error::Result,
  primop, primop2, primop3,
  thunk::{StaticScope, ThunkId},
  value::{PathSet, Value},
  Eval,
};
use std::collections::{BTreeSet, HashMap};
use syntax::expr::Ident;

#[derive(Default, Debug)]
struct Derivation<'a> {
  name: &'a str,
  builder: Option<&'a str>,
  system: Option<&'a str>,
  args: Vec<String>,
  env: HashMap<&'a str, String>,
  input_sources: PathSet,
}

pub fn derivation_strict(eval: &Eval, args: ThunkId) -> Result<Value> {
  let attrs = eval.value_attrs_of(args)?;

  let mut context = PathSet::new();

  let name = eval.value_string_of(
    attrs
      .get(&Ident::from("name"))
      .copied()
      .ok_or_else(|| anyhow::anyhow!("required attribute `name' missing"))?,
  )?;

  if attrs.contains_key(&Ident::from("__structuredAttrs")) {
    bail!("not implemented: structuredAttrs");
  }

  let mut drv = Derivation {
    name,
    ..Default::default()
  };

  let ignore_nulls = match attrs.get(&Ident::from("__ignoreNulls")) {
    Some(n) => eval.value_bool_of(*n)?,
    None => false,
  };

  let mut outputs_set: BTreeSet<&str> = vec!["out"].into_iter().collect();
  let mut is_recursive = false;
  let mut output_hash_algo = None;
  let mut output_hash = None;

  for (k, v) in attrs {
    trace!("processing attribute {}", k);

    if k == "__ignoreNulls" {
      continue;
    }

    if k == "args" {
      for arg in eval.value_list_of(*v)? {
        drv
          .args
          .push(coerce_to_string(eval, *arg, &mut context, true, false)?);
      }
      continue;
    }

    drv
      .env
      .insert(k, coerce_to_string(eval, *v, &mut context, true, false)?);

    if k == "outputHashMode" {
      is_recursive = match eval.value_string_of(*v)? {
        "recursive" => true,
        "flat" => false,
        x => bail!("invalid value `{}' for outputHashMode", x),
      };
    }

    if k == "outputHashAlgo" {
      output_hash_algo = Some(eval.value_string_of(*v)?);
    }

    if k == "outputHash" {
      output_hash = Some(eval.value_string_of(*v)?);
    }

    if k == "outputs" {
      outputs_set.clear();
      for name in eval.value_list_of(*v)? {
        let name = eval.value_string_of(*name)?;
        if name == "drv" {
          bail!("derivation outputs cannot be named `drv'");
        }
        if !outputs_set.insert(name) {
          bail!("duplicate derivation output `{}'", name);
        }
      }
      if outputs_set.is_empty() {
        bail!("derivation outputs list cannot be empty");
      }
    }

    if k == "builder" {
      drv.builder = Some(eval.value_with_context_of(*v)?.0);
    }
    if k == "system" {
      drv.system = Some(eval.value_with_context_of(*v)?.0);
    }
  }

  for path in &context {
    debug!("input source: {}", path.display());
  }

  debug!("{:?}", drv);

  bail!("derivation: unimplemented")
}
