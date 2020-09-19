use crate::{
  derivation::{Derivation, FixedOutputHash, Output},
  eval::{
    builtins::strings::coerce_to_string,
    context::StaticScope,
    thunk::ThunkId,
    value::{PathSet, Value},
    Eval,
  },
  hash::{Hash, HashType},
  store::{FileIngestionMethod, RepairFlag},
  syntax::expr::Ident,
  util::*,
};
use std::{
  collections::BTreeSet,
  path::{Path, PathBuf},
};

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
    name: name.to_string(),
    ..Default::default()
  };

  let ignore_nulls = match attrs.get(&Ident::from("__ignoreNulls")) {
    Some(n) => eval.value_bool_of(*n)?,
    None => false,
  };

  let mut outputs_set: BTreeSet<String> = std::iter::once(String::from("out")).collect();
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

    let string_value = coerce_to_string(eval, *v, &mut context, true, false)?;

    if ignore_nulls {
      if let Value::Null = eval.value_of(*v)? {
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
      drv.platform = eval.value_with_context_of(*v)?.0.to_string();
    }
  }

  for path in &context {
    if path.starts_with('=') {
      let mut refs = Default::default();
      eval.store.compute_closure(
        &eval.store.parse_store_path(Path::new(&path[1..]))?,
        &mut refs,
        false,
        false,
        false,
      )?;
      for r in refs {
        if r.is_derivation() {
          drv.input_derivations.insert(
            r.clone(),
            eval
              .store
              .read_derivation(&r)?
              .outputs
              .into_iter()
              .map(|x| x.0)
              .collect(),
          );
        }
        drv.input_sources.insert(r);
      }
    } else if path.starts_with('!') {
      let (path, name) = decode_context(path);
      drv.input_derivations.insert(
        eval.store.parse_store_path(path)?,
        std::iter::once(name.to_string()).collect(),
      );
    } else {
      drv
        .input_sources
        .insert(eval.store.parse_store_path(&Path::new(path))?);
    }
  }

  if drv.builder == PathBuf::from("") {
    bail!("required attribute `builder' missing");
  }

  if drv.platform.is_empty() {
    bail!("required attribute `system' missing");
  }

  if drv.name.ends_with(".drv") {
    bail!("derivation names may not end in `.drv'");
  }

  if let Some(h) = output_hash {
    if outputs_set.len() != 1 || !outputs_set.contains("out") {
      bail!("multiple outputs are not supported in fixed-output derivations");
    }

    let drv_hash = Hash::new_allow_empty(
      &h,
      output_hash_algo.and_then(|x| x.parse::<HashType>().ok()),
    )?;

    let out_path = eval.store.make_fixed_output_path(
      if is_recursive {
        FileIngestionMethod::Recursive
      } else {
        FileIngestionMethod::Flat
      },
      &drv_hash,
      name,
      &mut std::iter::empty(),
      false,
    )?;

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
          path: crate::path::DUMMY.clone(),
        },
      );
    }

    let drv_hash = eval.store.hash_derivation_modulo(&drv, true)?;

    for out in &outputs_set {
      let output_path = eval.store.make_fixed_output_path(
        if is_recursive {
          FileIngestionMethod::Recursive
        } else {
          FileIngestionMethod::Flat
        },
        &drv_hash,
        name,
        &mut std::iter::empty(),
        false,
      )?;
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

  let drv_path = eval
    .store
    .write_derivation(&drv, name, RepairFlag::NoRepair)?;
  let path_str = eval.store.print_store_path(&drv_path);

  info!("instantiated `{}' -> `{}'", name, path_str);

  let mut attrs = StaticScope::new();

  attrs.insert(
    Ident::from("drvPath"),
    eval.new_value(Value::String {
      string: path_str.clone(),
      context: std::iter::once(format!("={}", &path_str)).collect(),
    }),
  );

  for (name, out) in &drv.outputs {
    attrs.insert(
      Ident::from(name.as_str()),
      eval.new_value(Value::String {
        string: eval.store.print_store_path(&out.path),
        context: std::iter::once(format!("!{}!{}", &name, &path_str)).collect(),
      }),
    );
  }

  Ok(Value::AttrSet(attrs))
}
