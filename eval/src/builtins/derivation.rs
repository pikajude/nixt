use crate::{
  builtins::strings::coerce_to_string,
  thunk::{StaticScope, ThunkId},
  value::{PathSet, Value},
  Eval,
};
use async_std::path::{Path, PathBuf};
use nix_core::{
  derivation::{Derivation, FixedOutputHash, Output},
  hash::{Hash, HashType},
};
use nix_syntax::expr::Ident;
use nix_util::*;
use std::collections::BTreeSet;

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
    name: name.to_string(),
    ..Default::default()
  };

  let ignore_nulls = match attrs.get(&Ident::from("__ignoreNulls")) {
    Some(n) => eval.value_bool_of(*n).await?,
    None => false,
  };

  let mut outputs_set: BTreeSet<String> = vec![String::from("out")].into_iter().collect();
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

    let string_value = coerce_to_string(eval, *v, &mut context, true, false).await?;

    if ignore_nulls {
      if let Value::Null = eval.value_of(*v).await? {
        continue;
      }
    }

    drv.env.insert(k.to_string(), string_value.clone());

    if k == "outputHashMode" {
      is_recursive = match &string_value[..] {
        "recursive" => true,
        "flat" => false,
        x => bail!("invalid value `{}' for outputHashMode", x),
      };
    }

    if k == "outputHashAlgo" {
      output_hash_algo = Some(string_value.clone());
    }

    if k == "outputHash" {
      output_hash = Some(string_value.clone());
    }

    if k == "outputs" {
      outputs_set.clear();
      for name in string_value.split(' ') {
        if name == "drv" {
          bail!("derivation outputs cannot be named `drv'");
        }
        if !outputs_set.insert(name.to_string()) {
          bail!("duplicate derivation output `{}'", name);
        }
      }
      if outputs_set.is_empty() {
        bail!("derivation outputs list cannot be empty");
      }
    }

    if k == "builder" {
      drv.builder = drv
        .env
        .get("builder")
        .map(|x| Path::new(x.as_str()).to_path_buf())
        .unwrap();
    }
    if k == "system" {
      drv.platform = eval.value_with_context_of(*v).await?.0.to_string();
    }
  }

  for path in &context {
    debug!("input source: {}", path.display());
  }

  if let Some(h) = output_hash {
    if outputs_set.len() != 1 || !outputs_set.contains("out") {
      bail!("multiple outputs are not supported in fixed-output derivations");
    }

    let drv_hash = Hash::new_allow_empty(
      &h,
      output_hash_algo.and_then(|x| x.parse::<HashType>().ok()),
    )?;

    let out_path =
      eval
        .store
        .make_fixed_output_path(is_recursive, &drv_hash, name, vec![], false)?;

    let out_str = eval.store.print_store_path(&out_path);

    drv.env.insert("out".into(), out_str);
    drv.outputs.insert(
      "out".into(),
      Output {
        path: out_path,
        hash: Some(FixedOutputHash {
          recursive: is_recursive,
          hash: drv_hash,
        }),
      },
    );
  } else {
    for out in &outputs_set {
      drv.env.insert(out.to_string(), "".into());
      drv.outputs.insert(
        out.to_string(),
        Output {
          hash: None,
          path: nix_core::path::DUMMY.clone(),
        },
      );
    }

    let drv_hash = eval.store.hash_derivation_modulo(&drv, true).await?;

    for out in &outputs_set {
      let output_path =
        eval
          .store
          .make_fixed_output_path(is_recursive, &drv_hash, name, vec![], false)?;
      drv
        .env
        .insert(out.to_string(), eval.store.print_store_path(&output_path));
      drv.outputs.insert(
        out.to_string(),
        Output {
          path: output_path,
          hash: None,
        },
      );
    }
  }

  let mut attrs = StaticScope::new();

  for (name, out) in &drv.outputs {
    attrs.insert(
      Ident::from(name.as_str()),
      eval.new_value(Value::String {
        string: eval.store.print_store_path(&out.path),
        context: vec![PathBuf::from(format!("!{}!", &name))]
          .into_iter()
          .collect(),
      }),
    );
  }

  Ok(Value::AttrSet(attrs))
}
