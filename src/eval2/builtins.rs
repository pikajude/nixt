use super::*;
use std::cmp::Ordering;

#[derive(thiserror::Error, Debug)]
#[error("assertion failure: {}", _0)]
pub struct AssertionFailure(pub String);

impl Eval {
  pub fn attrnames_prim(&self, attrs: &ValueRef) -> Result<ValueRef> {
    Ok(arc(Value::List(
      self
        .value_attrs_of(attrs)?
        .keys()
        .map(|k| arc(Value::String((k.to_string(), Default::default()))))
        .collect(),
    )))
  }

  pub fn compare_versions(&self, lhs: &ValueRef, rhs: &ValueRef) -> Result<ValueRef> {
    let cmp = do_compare(&self.value_string_of(lhs)?, &self.value_string_of(rhs)?);
    Ok(arc(Value::Int(match cmp {
      Ordering::Less => -1,
      Ordering::Greater => 1,
      Ordering::Equal => 0,
    })))
  }

  pub fn find_file(&self, path: &ValueRef, filename: &str) -> Result<PathBuf> {
    let entries = self.value_list_of(path)?;
    let target = Path::new(filename);
    let mut path_parts = target.components();
    let search_key = path_parts
      .next()
      .expect("must have at least one item in path")
      .as_os_str();
    let has_children = path_parts.as_path().iter().next().is_some();
    let add_children = move |p: PathBuf| {
      if has_children {
        p.join(path_parts.as_path())
      } else {
        p
      }
    };
    for entry in entries.iter() {
      let kv = self.value_attrs_of(entry)?;
      let path = self.value_string_of(kv.get(&Ident::from("path")).unwrap())?;

      let (_context, prefix) = self.coerce_new_string(
        kv.get(&Ident::from("prefix")).unwrap(),
        CoerceOpts {
          extended: false,
          copy_to_store: false,
        },
      )?;

      if !_context.is_empty() {
        warn!("try to realise context {:?}", _context);
      }

      if search_key == &*prefix {
        let full = add_children(path.to_string().into());
        if full.exists() {
          return Ok(full);
        }
      } else if prefix.is_empty() {
        if let Ok(iter) = fs::read_dir(&*path) {
          for next_item in iter {
            let next_item = next_item?;
            if next_item.file_name() == search_key {
              return Ok(add_children(next_item.path()));
            }
          }
        }
      }
    }
    Err(anyhow!(AssertionFailure(format!(
      "Entry `{}' was not found in the Nix search path",
      target.display()
    ))))
  }

  pub fn foldl_strict(
    &self,
    oper: &ValueRef,
    seed: &ValueRef,
    list: &ValueRef,
  ) -> Result<ValueRef> {
    let items = self.value_list_of(list)?;
    if items.is_empty() {
      Ok(Arc::clone(seed))
    } else {
      let mut cur_item = Arc::clone(seed);
      for (i, thunk) in items.iter().enumerate() {
        let acc_fn = self.apply_function(oper, &cur_item)?;
        let next_item = self.apply_function(&acc_fn, thunk)?;
        if i + 1 == items.len() {
          return Ok(next_item);
        } else {
          cur_item = Arc::clone(&next_item);
        }
      }
      unreachable!()
    }
  }

  pub fn gen_list(&self, generator: &ValueRef, len: &ValueRef) -> Result<ValueRef> {
    let target_len = self.value_int_of(len)?;
    let target_len = if target_len < 0 {
      bail!("cannot create a list of size {}", target_len)
    } else {
      target_len as usize
    };
    let mut out_list = vec![];
    for ix in 0..target_len {
      let fn_arg = arc(Value::Int(ix as _));
      out_list.push(arc(Value::Apply(Arc::clone(generator), fn_arg)));
    }
    Ok(arc(Value::List(out_list)))
  }

  pub fn get_env(&self, var: &ValueRef) -> Result<ValueRef> {
    let v = self.value_string_of(var)?;
    Ok(arc(Value::String((
      std::env::var(&*v).unwrap_or_default(),
      Default::default(),
    ))))
  }

  pub fn import(&self, path: &ValueRef) -> Result<ValueRef> {
    let path = self.value_path_of(path)?;
    let meta = path.metadata()?;
    if meta.is_dir() {
      self.load_file(path.join("default.nix"))
    } else {
      self.load_file(path)
    }
  }

  pub fn intersect_attrs(&self, lhs: &ValueRef, rhs: &ValueRef) -> Result<ValueRef> {
    let attrs = self.value_attrs_of(lhs)?;
    let mut attrs2 = self.value_attrs_of(rhs)?.clone();
    attrs2
      .drain_filter(|key, _| !attrs.contains_key(key))
      .last();
    Ok(arc(Value::Attrs(attrs2)))
  }

  pub fn list_to_attrs(&self, list: &ValueRef) -> Result<ValueRef> {
    let mut attrs = StaticScope::new();
    let name_sym = Ident::from("name");
    let value_sym = Ident::from("value");
    for obj in self.value_list_of(list)?.iter() {
      let obj = self.value_attrs_of(obj)?;
      let name = self.value_string_of(
        obj
          .get(&name_sym)
          .ok_or_else(|| anyhow::anyhow!("Missing attribute name"))?,
      )?;
      let value = obj
        .get(&value_sym)
        .ok_or_else(|| anyhow::anyhow!("Missing attribute value"))?;
      attrs.insert(Ident::from(name.as_str()), Arc::clone(value));
    }
    Ok(arc(Value::Attrs(attrs)))
  }

  pub fn map_list(&self, op: &ValueRef, value: &ValueRef) -> Result<ValueRef> {
    let items = self.value_list_of(value)?;
    Ok(arc(Value::List(
      items
        .iter()
        .map(|x| arc(Value::Apply(Arc::clone(op), Arc::clone(x))))
        .collect(),
    )))
  }

  pub fn remove_attrs(&self, attrs: &ValueRef, remove: &ValueRef) -> Result<ValueRef> {
    let mut attrset = self.value_attrs_of(attrs)?.clone();
    for attr_name in self.value_list_of(remove)?.iter() {
      let remove_item = self.value_string_of(attr_name)?;
      attrset.remove(&Ident::from(remove_item.as_str()));
    }
    Ok(arc(Value::Attrs(attrset)))
  }

  pub fn replace_strings(
    &self,
    from: &ValueRef,
    to: &ValueRef,
    string: &ValueRef,
  ) -> Result<ValueRef> {
    let find_list = self.value_list_of(from)?;
    let replace_list = self.value_list_of(to)?;
    if find_list.len() != replace_list.len() {
      bail!("`from` and `to` lists must be the same length");
    }
    if find_list.is_empty() {
      return Ok(Arc::clone(string));
    }
    let mut froms = vec![];
    for item in find_list.iter() {
      froms.push(self.value_with_context_of(item)?);
    }
    let mut tos = vec![];
    for item in replace_list.iter() {
      tos.push(self.value_with_context_of(item)?);
    }
    let (rhs, mut rhs_context) = self.value_with_context_of(string)?.clone();
    let mut out = vec![];
    let bytes = rhs.as_bytes();
    let mut p = 0;
    while p <= bytes.len() {
      let mut found = false;
      for (i, f) in froms.iter().enumerate() {
        if bytes[p..].starts_with(f.0.as_bytes()) {
          found = true;
          out.extend(tos[i].0.as_bytes());
          rhs_context.extend(tos[i].1.iter().cloned());
          p += f.0.len();
          break;
        }
      }
      if !found {
        if p < bytes.len() {
          out.push(bytes[p]);
        }
        p += 1;
      }
    }
    Ok(arc(Value::String((
      String::from_utf8_lossy(&out).to_string(),
      rhs_context,
    ))))
  }
}

pub fn mk_nix_path() -> Value {
  let mut entries = vec![];
  for entry in get_nix_path().into_iter().chain(std::iter::once(format!(
    "nix={}/corepkgs",
    env!("CARGO_MANIFEST_DIR")
  ))) {
    let mut parts = entry.splitn(2, '=');
    let first = parts.next().unwrap();
    let second = parts.next();
    let (prefix, path) = match second {
      Some(x) => (first, x),
      None => ("", first),
    };
    let mut entry_attr = StaticScope::new();
    entry_attr.insert(
      "path".into(),
      arc(Value::String((path.into(), Default::default()))),
    );
    entry_attr.insert(
      "prefix".into(),
      arc(Value::String((prefix.into(), Default::default()))),
    );
    entries.push(arc(Value::Attrs(entry_attr)));
  }
  Value::List(entries)
}

fn parse_nix_path(n: &str) -> Vec<&str> {
  let mut strings = vec![];
  let mut start = 0;
  let mut prev_colon = 0;
  for (next_colon, _) in n.match_indices(':') {
    if let Some(x) = n[prev_colon..next_colon].rfind('=') {
      if is_uri(&n[prev_colon + x + 1..]) {
        prev_colon = next_colon;
        continue;
      }
    }
    strings.push(&n[start..next_colon]);
    start = next_colon + 1;
  }
  if start < n.len() {
    strings.push(&n[start..]);
  }
  strings
}

fn get_nix_path() -> Vec<String> {
  if let Ok(n) = std::env::var("NIX_PATH") {
    parse_nix_path(&n)
      .into_iter()
      .map(|x| x.to_string())
      .collect()
  } else {
    vec![]
  }
}

fn is_uri(s: &str) -> bool {
  [
    "http://",
    "https://",
    "file://",
    "channel:",
    "channel://",
    "git://",
    "s3://",
    "ssh://",
  ]
  .iter()
  .any(|x| s.starts_with(x))
}

fn do_compare(s1: &str, s2: &str) -> Ordering {
  trace!("compareVersions {:?} {:?}", s1, s2);
  let mut iter1 = s1.split(|p| p == '.' || p == '-');
  let mut iter2 = s2.split(|p| p == '.' || p == '-');
  loop {
    let num1 = iter1.next();
    let num2 = iter2.next();
    if num1.is_none() && num2.is_none() {
      break Ordering::Equal;
    }
    let c1 = num1.unwrap_or("");
    let c2 = num2.unwrap_or("");
    if components_lt(c1, c2) {
      break Ordering::Less;
    } else if components_lt(c2, c1) {
      break Ordering::Greater;
    }
  }
}

fn components_lt(s1: &str, s2: &str) -> bool {
  let num1 = s1.parse::<i64>().ok();
  let num2 = s2.parse::<i64>().ok();
  if let (Some(n1), Some(n2)) = (num1, num2) {
    n1 < n2
  } else if s1 == "" && num2.is_some() || s1 == "pre" && s2 != "pre" {
    true
  } else if s2 == "pre" {
    false
  } else if num1.is_some() {
    true
  } else if num2.is_some() {
    false
  } else {
    s1 < s2
  }
}
