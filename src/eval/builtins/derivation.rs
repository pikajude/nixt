use super::strings::coerce_to_string;
use crate::{
  bail,
  error::Result,
  eval::Eval,
  hash::{Hash, HashType},
  thunk::ThunkId,
  value::{PathSet, Value},
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

pub async fn derivation_strict(eval: &Eval, args: ThunkId) -> Result<Value> {
  let attrs = eval.value_attrs_of(args).await?;

  let mut context = PathSet::new();

  let name = eval
    .value_string_of(
      attrs
        .get(&Ident::from("name"))
        .copied()
        .ok_or_else(|| anyhow::anyhow!("required attribute `name' missing"))?,
    )
    .await?;

  if attrs.contains_key(&Ident::from("__structuredAttrs")) {
    bail!("not implemented: structuredAttrs");
  }

  let mut drv = Derivation {
    name,
    ..Default::default()
  };

  let ignore_nulls = match attrs.get(&Ident::from("__ignoreNulls")) {
    Some(n) => eval.value_bool_of(*n).await?,
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
      for arg in eval.value_list_of(*v).await? {
        drv
          .args
          .push(coerce_to_string(eval, *arg, &mut context, true, false).await?);
      }
      continue;
    }

    drv.env.insert(
      k,
      coerce_to_string(eval, *v, &mut context, true, false).await?,
    );

    if k == "outputHashMode" {
      is_recursive = match eval.value_string_of(*v).await? {
        "recursive" => true,
        "flat" => false,
        x => bail!("invalid value `{}' for outputHashMode", x),
      };
    }

    if k == "outputHashAlgo" {
      output_hash_algo = Some(eval.value_string_of(*v).await?);
    }

    if k == "outputHash" {
      output_hash = Some(eval.value_string_of(*v).await?);
    }

    if k == "outputs" {
      outputs_set.clear();
      for name in eval.value_list_of(*v).await? {
        let name = eval.value_string_of(*name).await?;
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
      drv.builder = Some(eval.value_with_context_of(*v).await?.0);
    }
    if k == "system" {
      drv.system = Some(eval.value_with_context_of(*v).await?.0);
    }
  }

  for path in &context {
    debug!("input source: {}", path.display());
  }

  if let Some(h) = output_hash {
    if outputs_set.len() != 1 || !outputs_set.contains("out") {
      bail!("multiple outputs are not supported in fixed-output derivations");
    }

    let drv_hash =
      Hash::new_allow_empty(h, output_hash_algo.and_then(|x| x.parse::<HashType>().ok()))?;

    debug!("{:?}", drv_hash);
  }

  debug!("{:?}", drv);

  bail!("derivation: unimplemented")
}
