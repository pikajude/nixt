use crate::{
  archive::PathFilter,
  derivation::hash_placeholder,
  eval::{
    context::StaticScope,
    thunk::ThunkId,
    value::{PathSet, Value},
    Eval,
  },
  hash::HashType,
  prelude::StorePath,
  store::{FileIngestionMethod, RepairFlag},
  syntax::expr::Ident,
  util::*,
  Store,
};
use lazy_static::lazy_static;
use parking_lot::Mutex;
use regex::Regex;
use std::{
  collections::{BTreeSet, HashMap},
  path::{Path, PathBuf},
  sync::Arc,
};

lazy_static! {
  static ref REGEX_CACHE: Mutex<HashMap<String, Arc<Regex>>> = Default::default();
  static ref IMPORTED_PATHS: Mutex<HashMap<PathBuf, StorePath>> = Default::default();
}

impl IMPORTED_PATHS {
  fn copy_to_store<P: AsRef<Path>, S: Store>(&self, store: &S, path: P) -> Result<StorePath> {
    let path = path.as_ref();
    if let Some(p) = self.lock().get(path) {
      return Ok(p.clone());
    }
    debug!("importing path {} into the store", path.display());
    let new_path = store.add_to_store_from_path(
      &*path
        .file_name()
        .ok_or_else(|| anyhow!("invalid imported path name"))?
        .to_string_lossy(),
      path,
      FileIngestionMethod::Recursive,
      HashType::SHA256,
      &PathFilter::none(),
      RepairFlag::NoRepair,
    )?;
    self.lock().insert(path.to_owned(), new_path.clone());
    Ok(new_path)
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct CoerceOpts {
  pub extended: bool,
  pub copy_to_store: bool,
}

impl CoerceOpts {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn extended(mut self) -> Self {
    self.extended = true;
    self
  }

  pub fn dont_copy(mut self) -> Self {
    self.copy_to_store = false;
    self
  }
}

impl Default for CoerceOpts {
  fn default() -> Self {
    Self {
      extended: false,
      copy_to_store: true,
    }
  }
}

fn make_regex(s: &str) -> Result<Arc<Regex>> {
  let mut m = REGEX_CACHE.lock();
  match m.get(s) {
    Some(x) => Ok(Arc::clone(x)),
    None => {
      m.insert(s.into(), Arc::new(Regex::new(s)?));
      Ok(Arc::clone(&m[s]))
    }
  }
}

pub fn substring(eval: &Eval, start: ThunkId, len: ThunkId, string: ThunkId) -> Result<Value> {
  let (ctx, s) = coerce_new_string(eval, string, Default::default())?;
  let start = eval.value_int_of(start)?;
  if start < 0 {
    bail!("first argument to `substring' must be >= 0");
  }
  let start = start as usize;
  let len = eval.value_int_of(len)?;
  let actual_end = std::cmp::min(start + (std::cmp::max(0, len) as usize), s.len());
  Ok(Value::String {
    string: s[start..actual_end].to_string(),
    context: ctx,
  })
}

pub fn prim_to_string(eval: &Eval, obj: ThunkId) -> Result<Value> {
  let mut ctx = PathSet::new();
  Ok(Value::String {
    string: coerce_to_string(
      eval,
      obj,
      &mut ctx,
      CoerceOpts {
        extended: true,
        copy_to_store: false,
      },
    )?,
    context: ctx,
  })
}

pub fn coerce_new_string(eval: &Eval, obj: ThunkId, opts: CoerceOpts) -> Result<(PathSet, String)> {
  let mut p = PathSet::new();
  let string = coerce_to_string(eval, obj, &mut p, opts)?;
  Ok((p, string))
}

pub fn coerce_to_string(
  eval: &Eval,
  obj: ThunkId,
  ctx: &mut PathSet,
  opts: CoerceOpts,
) -> Result<String> {
  let v = eval.value_of(obj)?;
  Ok(match v {
    Value::Path(p) => {
      if opts.copy_to_store {
        let storepath = IMPORTED_PATHS.copy_to_store(&*eval.store, p)?;
        let str_path = eval.store.print_store_path(&storepath);
        ctx.insert(str_path.clone());
        str_path
      } else {
        p.display().to_string()
      }
    }
    Value::String { string, context } => {
      ctx.extend(context.iter().cloned());
      string.clone()
    }
    Value::Int(i) => i.to_string(),
    Value::Bool(b) if opts.extended => {
      if *b {
        "1".into()
      } else {
        String::new()
      }
    }
    Value::Null if opts.extended => String::new(),
    Value::List(items) if opts.extended => {
      let mut output = String::new();
      for (i, item) in items.iter().enumerate() {
        if i > 0 {
          output.push(' ');
        }
        output.push_str(&coerce_to_string(eval, *item, ctx, opts)?);
      }
      output
    }
    Value::AttrSet(a) => {
      if let Some(o) = a.get(&Ident::from("outPath")) {
        coerce_to_string(eval, *o, ctx, opts)?
      } else {
        bail!("cannot coerce a set to a string unless it has an `outPath' attribute")
      }
    }
    v => bail!("cannot convert {} to a string", v.typename()),
  })
}

pub fn concat_strings_sep(eval: &Eval, sep: ThunkId, strings: ThunkId) -> Result<Value> {
  let strings = eval.value_list_of(strings)?;
  if strings.is_empty() {
    return Ok(Value::string_bare(""));
  }
  let mut all_ctx = BTreeSet::new();
  let mut output = String::new();
  let (sep, pset) = eval.value_with_context_of(sep)?;
  all_ctx.extend(pset.iter().cloned());

  for (ix, s) in strings.iter().enumerate() {
    if ix > 0 {
      output.push_str(sep);
    }
    output.push_str(&coerce_to_string(
      eval,
      *s,
      &mut all_ctx,
      Default::default(),
    )?);
  }

  Ok(Value::String {
    string: output,
    context: all_ctx,
  })
}

pub fn matches(eval: &Eval, regex: ThunkId, haystack: ThunkId) -> Result<Value> {
  let regex = eval.value_string_of(regex)?;
  let regex = make_regex(regex)?;
  let (s, _) = eval.value_with_context_of(haystack)?;
  if let Some(caps) = regex.captures(s) {
    let mut matchlist = vec![];
    for m in caps.iter().skip(1) {
      if let Some(m_) = m {
        matchlist.push(eval.new_value(Value::string_bare(m_.as_str())));
      } else {
        matchlist.push(eval.new_value(Value::Null));
      }
    }
    Ok(Value::List(matchlist))
  } else {
    Ok(Value::Null)
  }
}

pub fn split(eval: &Eval, pattern: ThunkId, string: ThunkId) -> Result<Value> {
  let pattern = eval.value_string_of(pattern)?;
  let pattern = make_regex(pattern)?;
  let (haystack, _) = eval.value_with_context_of(string)?;
  let mut items = vec![];
  for (i, match_) in pattern.split(haystack).enumerate() {
    if i > 0 {
      items.push(eval.new_value(Value::List(vec![])));
    }
    items.push(eval.new_value(Value::string_bare(match_)));
  }
  Ok(Value::List(items))
}

pub fn replace_strings(
  eval: &Eval,
  find: ThunkId,
  replace: ThunkId,
  string: ThunkId,
) -> Result<Value> {
  let findl = eval.value_list_of(find)?;
  let replacel = eval.value_list_of(replace)?;
  if findl.len() != replacel.len() {
    bail!("`from' and `to' list lengths don't match.")
  }
  let mut froms = vec![];
  for item in findl {
    froms.push(eval.value_with_context_of(*item)?.0);
  }
  let mut tos = vec![];
  for item in replacel {
    tos.push(eval.value_with_context_of(*item)?);
  }
  let (rhs, rhs_context) = eval.value_with_context_of(string)?;
  let mut out: Vec<u8> = vec![];
  let mut rhs_context = rhs_context.clone();
  let bytes = rhs.as_bytes();
  let mut p = 0;
  while p <= bytes.len() {
    let mut found = false;
    for (i, f) in froms.iter().enumerate() {
      if bytes[p..].starts_with(f.as_bytes()) {
        found = true;
        out.extend(tos[i].0.as_bytes());
        rhs_context.extend(tos[i].1.iter().cloned());
        p += f.len();
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
  Ok(Value::String {
    string: String::from_utf8_lossy(&out).to_string(),
    context: rhs_context,
  })
}

pub fn parse_drv_name(eval: &Eval, name: ThunkId) -> Result<Value> {
  let name = eval.value_string_of(name)?;
  let mut breakpoint = name.len();
  let mut iter = name.char_indices().peekable();
  while let Some((i, ch)) = iter.next() {
    if ch == '-' && iter.peek().map_or(false, |(_, c)| !c.is_alphanumeric()) {
      breakpoint = i;
      break;
    }
  }
  let (name, version) = name.split_at(breakpoint);
  Ok(Value::AttrSet({
    let mut a = StaticScope::new();
    a.insert("name".into(), eval.new_value(Value::string_bare(name)));
    a.insert(
      "version".into(),
      eval.new_value(Value::string_bare(version)),
    );
    a
  }))
}

pub fn placeholder(eval: &Eval, name: ThunkId) -> Result<Value> {
  Ok(Value::string_bare(hash_placeholder(
    eval.value_string_of(name)?,
  )))
}

pub fn discard_context(eval: &Eval, string: ThunkId) -> Result<Value> {
  Ok(Value::string_bare(eval.value_with_context_of(string)?.0))
}

pub fn basename_of(eval: &Eval, string: ThunkId) -> Result<Value> {
  let (s, c) = eval.value_with_context_of(string)?;
  if let Some(x) = std::path::Path::new(s).file_name() {
    Ok(Value::String {
      string: x.to_string_lossy().to_string(),
      context: c.clone(),
    })
  } else {
    bail!("invalid path as input")
  }
}
