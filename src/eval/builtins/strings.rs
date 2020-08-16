use crate::{
  derivation::hash_placeholder,
  eval::{
    context::StaticScope,
    thunk::ThunkId,
    value::{PathSet, Value},
    Eval,
  },
  syntax::expr::Ident,
  util::*,
};
use lazy_static::lazy_static;
use regex::Regex;
use std::{
  collections::{BTreeSet, HashMap},
  sync::{Arc, Mutex},
};

lazy_static! {
  static ref REGEX_CACHE: Mutex<HashMap<String, Arc<Regex>>> = Default::default();
}

fn make_regex(s: &str) -> Result<Arc<Regex>> {
  let mut m = REGEX_CACHE.lock().unwrap();
  match m.get(s) {
    Some(x) => Ok(Arc::clone(x)),
    None => {
      m.insert(s.into(), Arc::new(Regex::new(s)?));
      Ok(Arc::clone(&m[s]))
    }
  }
}

pub async fn substring(
  eval: &Eval,
  start: ThunkId,
  len: ThunkId,
  string: ThunkId,
) -> Result<Value> {
  let (ctx, s) = coerce_new_string(eval, string, false, false).await?;
  let start = eval.value_int_of(start).await?;
  if start < 0 {
    bail!("first argument to `substring' must be >= 0");
  }
  let start = start as usize;
  let len = eval.value_int_of(len).await?;
  let actual_end = std::cmp::min(start + (std::cmp::max(0, len) as usize), s.len());
  Ok(Value::String {
    string: s[start..actual_end].to_string(),
    context: ctx,
  })
}

pub async fn prim_to_string(eval: &Eval, obj: ThunkId) -> Result<Value> {
  let mut ctx = PathSet::new();
  Ok(Value::String {
    string: coerce_to_string(eval, obj, &mut ctx, true, false).await?,
    context: ctx,
  })
}

pub async fn coerce_new_string(
  eval: &Eval,
  obj: ThunkId,
  extended: bool,
  copy_to_store: bool,
) -> Result<(PathSet, String)> {
  let mut p = PathSet::new();
  let string = coerce_to_string(eval, obj, &mut p, extended, copy_to_store).await?;
  Ok((p, string))
}

#[async_recursion]
pub async fn coerce_to_string(
  eval: &Eval,
  obj: ThunkId,
  ctx: &mut PathSet,
  extended: bool,
  copy_to_store: bool,
) -> Result<String> {
  let v = eval.value_of(obj).await?;
  Ok(match v {
    Value::Path(p) => p.display().to_string(),
    Value::String { string, context } => {
      ctx.extend(context.iter().cloned());
      string.clone()
    }
    Value::Int(i) => i.to_string(),
    Value::Bool(b) if extended => {
      if *b {
        "1".into()
      } else {
        String::new()
      }
    }
    Value::Null if extended => String::new(),
    Value::List(items) if extended => {
      let mut output = String::new();
      for (i, item) in items.iter().enumerate() {
        if i > 0 {
          output.push(' ');
        }
        output.push_str(&coerce_to_string(eval, *item, ctx, extended, copy_to_store).await?);
      }
      output
    }
    Value::AttrSet(a) => {
      if let Some(o) = a.get(&Ident::from("outPath")) {
        coerce_to_string(eval, *o, ctx, extended, copy_to_store).await?
      } else {
        bail!("cannot coerce a set to a string unless it has an `outPath' attribute")
      }
    }
    v => bail!("cannot convert {} to a string", v.typename()),
  })
}

pub async fn concat_strings_sep(eval: &Eval, sep: ThunkId, strings: ThunkId) -> Result<Value> {
  let strings = eval.value_list_of(strings).await?;
  if strings.is_empty() {
    return Ok(Value::string_bare(""));
  }
  let mut all_ctx = BTreeSet::new();
  let mut output = String::new();
  let (sep, pset) = eval.value_with_context_of(sep).await?;
  all_ctx.extend(pset.iter().cloned());

  for (ix, s) in strings.iter().enumerate() {
    if ix > 0 {
      output.push_str(sep);
    }
    output.push_str(&coerce_to_string(eval, *s, &mut all_ctx, false, false).await?);
  }

  Ok(Value::String {
    string: output,
    context: all_ctx,
  })
}

pub async fn matches(eval: &Eval, regex: ThunkId, haystack: ThunkId) -> Result<Value> {
  let regex = eval.value_string_of(regex).await?;
  let regex = make_regex(regex)?;
  let (s, _) = eval.value_with_context_of(haystack).await?;
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

pub async fn split(eval: &Eval, pattern: ThunkId, string: ThunkId) -> Result<Value> {
  let pattern = eval.value_string_of(pattern).await?;
  let pattern = make_regex(pattern)?;
  let (haystack, _) = eval.value_with_context_of(string).await?;
  let mut items = vec![];
  for (i, match_) in pattern.split(haystack).enumerate() {
    if i > 0 {
      items.push(eval.new_value(Value::List(vec![])));
    }
    items.push(eval.new_value(Value::string_bare(match_)));
  }
  Ok(Value::List(items))
}

pub async fn replace_strings(
  eval: &Eval,
  find: ThunkId,
  replace: ThunkId,
  string: ThunkId,
) -> Result<Value> {
  let findl = eval.value_list_of(find).await?;
  let replacel = eval.value_list_of(replace).await?;
  if findl.len() != replacel.len() {
    bail!("`from' and `to' list lengths don't match.")
  }
  let mut froms = vec![];
  for item in findl {
    froms.push(eval.value_with_context_of(*item).await?.0);
  }
  let mut tos = vec![];
  for item in replacel {
    tos.push(eval.value_with_context_of(*item).await?);
  }
  let (rhs, rhs_context) = eval.value_with_context_of(string).await?;
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
      p += 1;
    }
  }
  Ok(Value::String {
    string: String::from_utf8_lossy(bytes).to_string(),
    context: rhs_context,
  })
}

pub async fn parse_drv_name(eval: &Eval, name: ThunkId) -> Result<Value> {
  let name = eval.value_string_of(name).await?;
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

pub async fn placeholder(eval: &Eval, name: ThunkId) -> Result<Value> {
  Ok(Value::string_bare(hash_placeholder(
    eval.value_string_of(name).await?,
  )))
}

pub async fn discard_context(eval: &Eval, string: ThunkId) -> Result<Value> {
  Ok(Value::string_bare(
    eval.value_with_context_of(string).await?.0,
  ))
}
