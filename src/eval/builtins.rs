use super::{error::Catchable, CoerceOpts, Eval};
use crate::{prelude::*, syntax::expr::LambdaArg, value::*};
use itertools::Either;
use parking_lot::Mutex;
use regex::Regex;
use serde_json::Serializer;
use std::{
  borrow::Cow,
  cmp,
  collections::{BTreeSet, HashMap, HashSet},
  convert::TryInto,
  path::{Path, PathBuf},
  sync::Arc,
};

const NIX_VERSION: &str = "2.7";

lazy_static! {
  static ref REGEXES: Mutex<HashMap<String, Arc<Regex>>> = Default::default();
}

impl REGEXES {
  fn get<S: AsRef<str>>(&self, regex: &S) -> Result<Arc<Regex>> {
    let regex = regex.as_ref();
    let mut l = self.lock();
    if let Some(r) = l.get(regex) {
      return Ok(r.clone());
    }
    let r = Arc::new(Regex::new(regex)?);
    l.insert(regex.to_string(), Arc::clone(&r));
    Ok(r)
  }
}

macro_rules! checktype {
  ($s:expr, $name:literal, $( $pattern:pat )|+ $( if $guard: expr )? $(,)?) => {
    $s.add_primop(
      concat!("__is", $name),
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        Ok(Value::Bool(matches!(
          &*eval.force_value(&args[0], pos)?,
          $($pattern)|+ $(if $guard)?
        )))
      },
    )
  };
}

impl Eval {
  pub fn create_base_env(&self) -> Result<()> {
    self.add_constant("builtins", writable(Value::Attrs(readable(Attrs::new()))))?;
    self.add_constant("true", writable(Value::Bool(true)))?;
    self.add_constant("false", writable(Value::Bool(false)))?;
    self.add_constant("null", writable(Value::Null))?;

    self.add_constant(
      "__currentSystem",
      writable(Value::string_bare("x86_64-linux")),
    )?;

    self.add_constant(
      "__nixVersion",
      writable(Value::String(Str {
        s: NIX_VERSION.into(),
        context: Default::default(),
      })),
    )?;

    let scoped_import = self.add_primop("scopedImport", 2, prim_scoped_import)?;
    let unscoped_import = writable(Value::Apply(
      scoped_import,
      writable(Value::Attrs(Default::default())),
    ));
    let _ = self.force_value(&unscoped_import, Pos::none())?;
    self.add_constant("import", unscoped_import)?;

    checktype!(self, "Attrs", Value::Attrs{..})?;
    checktype!(self, "Bool", Value::Bool{..})?;
    checktype!(self, "List", Value::List{..})?;
    checktype!(self, "Function", Value::Primop{..} | Value::Lambda{..})?;
    checktype!(self, "String", Value::String{..})?;

    self.add_primop("throw", 1, prim_throw)?;
    self.add_primop("abort", 1, prim_abort)?;
    self.add_primop("__tryEval", 1, prim_try_eval)?;
    self.add_primop("toString", 1, prim_to_string)?;
    self.add_primop("__intersectAttrs", 2, prim_intersect_attrs)?;
    self.add_primop("__getEnv", 1, prim_get_env)?;
    self.add_primop("__pathExists", 1, prim_path_exists)?;
    self.add_primop("__compareVersions", 2, prim_compare_versions)?;
    self.add_primop("baseNameOf", 1, prim_basename_of)?;
    self.add_primop("dirOf", 1, prim_dir_of)?;
    self.add_primop("removeAttrs", 2, prim_remove_attrs)?;
    self.add_primop("__listToAttrs", 1, prim_list_to_attrs)?;
    self.add_primop("__length", 1, prim_length)?;
    self.add_primop("__stringLength", 1, prim_string_length)?;
    self.add_primop("__substring", 3, prim_substring)?;
    self.add_primop("__elemAt", 2, prim_elem_at)?;
    self.add_primop("__head", 1, prim_head)?;
    self.add_primop("__tail", 1, prim_tail)?;
    self.add_primop("__seq", 2, prim_seq)?;
    self.add_primop("__elem", 2, prim_elem)?;
    self.add_primop("__genericClosure", 1, prim_generic_closure)?;
    self.add_primop("__concatLists", 1, prim_concat_lists)?;
    self.add_primop(
      "__unsafeDiscardStringContext",
      1,
      prim_unsafe_discard_string_context,
    )?;
    self.add_primop("__attrNames", 1, prim_attr_names)?;
    self.add_primop("__mapAttrs", 2, prim_map_attrs)?;
    self.add_primop("__genList", 2, prim_gen_list)?;
    self.add_primop("__functionArgs", 1, prim_function_args)?;
    self.add_primop("__addErrorContext", 2, prim_add_error_context)?;
    self.add_primop("map", 2, prim_map)?;
    self.add_primop("__filter", 2, prim_filter)?;
    self.add_primop("__lessThan", 2, prim_less_than)?;
    self.add_primop("__sub", 2, prim_sub)?;
    self.add_primop("__mul", 2, prim_mul)?;
    self.add_primop("__div", 2, prim_div)?;
    self.add_primop("__split", 2, prim_split)?;
    self.add_primop("__concatStringsSep", 2, prim_concat_strings_sep)?;
    self.add_primop("__toJSON", 1, prim_to_json)?;
    self.add_primop("__findFile", 2, prim_find_file)?;
    self.add_constant("__nixPath", writable(mk_nix_path()))?;

    self.add_primop(
      "derivationStrict",
      1,
      super::derivation::prim_derivation_strict,
    )?;
    let corepkg_derivation = self.eval_file(concat!(
      env!("CARGO_MANIFEST_DIR"),
      "/corepkgs/derivation.nix"
    ))?;
    self.add_constant("derivation", writable(corepkg_derivation))?;

    Ok(())
  }

  pub(super) fn coerce_new_string(
    &self,
    pos: Pos,
    value: &ValueRef,
    opts: CoerceOpts,
  ) -> Result<Str> {
    let mut ctx = PathSet::new();
    let s = self.coerce_to_string(pos, value, &mut ctx, opts)?;
    Ok(Str { s, context: ctx })
  }

  pub(super) fn coerce_to_path(
    &self,
    pos: Pos,
    value: &ValueRef,
    context: &mut PathSet,
  ) -> Result<PathBuf> {
    let s = self.coerce_to_string(
      pos,
      value,
      context,
      CoerceOpts {
        copy_to_store: false,
        coerce_more: false,
      },
    )?;
    let p = Path::new(&s);
    if !p.is_absolute() {
      throw!(pos, "string `{}' does not represent an absolute path", s);
    }
    Ok(p.to_path_buf())
  }

  pub(super) fn coerce_to_string(
    &self,
    pos: Pos,
    value: &ValueRef,
    context: &mut PathSet,
    opts: CoerceOpts,
  ) -> Result<String> {
    Ok(match &*self.force_value(value, pos)? {
      Value::String(Str { context: c1, s }) => {
        context.extend(c1.clone());
        s.clone()
      }
      Value::Path(p) => {
        let p = p.canonicalize()?;
        if opts.copy_to_store {
          warn!("must copy {} into the store", p.display())
        }
        p.display().to_string()
      }
      Value::Attrs(a) => {
        if let Some(x) = a.get(&ident!("outPath")) {
          return self.coerce_to_string(pos, &x.v, context, opts);
        } else {
          throw!(pos, "cannot coerce a set to a string")
        }
      }
      Value::Bool(b) if opts.coerce_more => {
        if *b {
          "1".into()
        } else {
          "".into()
        }
      }
      Value::Int(i) if opts.coerce_more => i.to_string(),
      Value::Float(f) if opts.coerce_more => f.to_string(),
      Value::Null if opts.coerce_more => "".into(),
      Value::List(ls) => {
        let mut s = String::new();
        for (i, item) in ls.iter().enumerate() {
          if i > 0 {
            s.push(' ');
          }
          s.push_str(self.coerce_to_string(pos, &item, context, opts)?.as_str());
        }
        s
      }
      v => throw!(pos, "cannot coerce {} to a string", v.typename()),
    })
  }
}

fn compare_versions(s1: &str, s2: &str) -> cmp::Ordering {
  let mut iter1 = s1.split(|p| p == '.' || p == '-');
  let mut iter2 = s2.split(|p| p == '.' || p == '-');
  loop {
    let num1 = iter1.next();
    let num2 = iter2.next();
    if num1.is_none() && num2.is_none() {
      break cmp::Ordering::Equal;
    }
    let c1 = num1.unwrap_or("");
    let c2 = num2.unwrap_or("");
    if components_lt(c1, c2) {
      break cmp::Ordering::Less;
    } else if components_lt(c2, c1) {
      break cmp::Ordering::Greater;
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

pub struct CompareValues<'a>(pub Cow<'a, Value>);

impl PartialEq for CompareValues<'_> {
  fn eq(&self, other: &Self) -> bool {
    if let Some(pair) = super::numbers(&*self.0, &*other.0) {
      match pair {
        Either::Left((a, b)) => a == b,
        Either::Right((a, b)) => a == b,
      }
    } else {
      match (&*self.0, &*other.0) {
        (Value::String(Str { s: s1, .. }), Value::String(Str { s: s2, .. })) => s1 == s2,
        (Value::Path(p1), Value::Path(p2)) => p1 == p2,
        _ => false,
      }
    }
  }
}

impl Eq for CompareValues<'_> {}

impl PartialOrd for CompareValues<'_> {
  fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
    if let Some(pair) = super::numbers(&self.0, &other.0) {
      match pair {
        Either::Left((a, b)) => a.partial_cmp(&b),
        Either::Right((a, b)) => a.partial_cmp(&b),
      }
    } else {
      match (&*self.0, &*other.0) {
        (Value::String(Str { s: s1, .. }), Value::String(Str { s: s2, .. })) => s1.partial_cmp(s2),
        (Value::Path(p1), Value::Path(p2)) => p1.partial_cmp(p2),
        _ => None,
      }
    }
  }
}

// We must fix this somehow
impl Ord for CompareValues<'_> {
  fn cmp(&self, other: &Self) -> cmp::Ordering {
    self
      .partial_cmp(other)
      .expect("incomparable values in CompareValues")
  }
}

fn prim_concat_strings_sep(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut out = String::new();
  let mut ctx = PathSet::new();

  let sep = eval.force_string(&args[0], pos)?;
  let strings = eval.force_list(&args[1], pos)?;

  for (i, item) in strings.iter().enumerate() {
    if i > 0 {
      out.push_str(&sep.s);
    }
    out.push_str(
      &eval
        .coerce_to_string(pos, &item, &mut ctx, CoerceOpts::default())?
        .as_str(),
    );
  }

  Ok(Value::String(Str {
    s: out,
    context: ctx,
  }))
}

fn prim_split(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let regex = eval.force_string_no_context(&args[0], pos)?;
  let reg = REGEXES.get(&&*regex)?;

  let mut items = vec![];
  let haystack = eval.force_string(&args[1], pos)?;

  let mut prev_end = 0;

  for cap in reg.captures_iter(&haystack.s) {
    let capture_range = cap.get(0).expect("captures always have at least one group");
    items.push(writable(Value::string_bare(
      &haystack.s[prev_end..capture_range.start()],
    )));
    prev_end = capture_range.end();
    items.push(writable(Value::List(readable(
      cap
        .iter()
        .skip(1)
        .filter_map(|x| x.map(|y| writable(Value::string_bare(y.as_str()))))
        .collect::<Vec<_>>(),
    ))));
  }

  items.push(writable(Value::string_bare(&haystack.s[prev_end..])));

  Ok(Value::List(readable(items)))
}

fn prim_div(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let v1 = eval.force_value(&args[0], pos)?;
  let v2 = eval.force_value(&args[1], pos)?;

  if let Some(pair) = super::numbers(&*v1, &*v2) {
    Ok(match pair {
      Either::Left((a, b)) => {
        if b == 0 {
          throw!(pos, "division by zero")
        }
        Value::Int(a / b)
      }
      Either::Right((a, b)) => {
        if b == 0.0 {
          throw!(pos, "division by zero")
        }
        Value::Float(a / b)
      }
    })
  } else {
    throw!(pos, "cannot divide {} and {}", v2.typename(), v1.typename())
  }
}

fn prim_mul(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let v1 = eval.force_value(&args[0], pos)?;
  let v2 = eval.force_value(&args[1], pos)?;

  if let Some(pair) = super::numbers(&*v1, &*v2) {
    Ok(match pair {
      Either::Left((a, b)) => Value::Int(a * b),
      Either::Right((a, b)) => Value::Float(a * b),
    })
  } else {
    throw!(
      pos,
      "cannot multiply {} and {}",
      v2.typename(),
      v1.typename()
    )
  }
}

fn prim_sub(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let v1 = eval.force_value(&args[0], pos)?;
  let v2 = eval.force_value(&args[1], pos)?;

  if let Some(pair) = super::numbers(&*v1, &*v2) {
    Ok(match pair {
      Either::Left((a, b)) => Value::Int(a - b),
      Either::Right((a, b)) => Value::Float(a - b),
    })
  } else {
    throw!(
      pos,
      "cannot subtract {} from {}",
      v2.typename(),
      v1.typename()
    )
  }
}

fn prim_less_than(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let v1 = eval.force_value(&args[0], pos)?;
  let v2 = eval.force_value(&args[1], pos)?;

  let v1 = CompareValues(Cow::Borrowed(&*v1));
  let v2 = CompareValues(Cow::Borrowed(&*v2));

  if let Some(res) = v1.partial_cmp(&v2) {
    Ok(Value::Bool(res == cmp::Ordering::Less))
  } else {
    bail!(
      "cannot compare {} with {}",
      v1.0.typename(),
      v2.0.typename()
    )
  }
}

fn prim_filter(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let items = eval.force_list(&args[1], pos)?;

  let mut new_list = vec![];

  for item in items.iter() {
    let result = match eval.call_function(&args[0], &item, pos)? {
      Value::Bool(b) => b,
      v => throw!(
        pos,
        "closure to `filter' should produce bool, not {}",
        v.typename()
      ),
    };
    if result {
      new_list.push(item.clone());
    }
  }

  Ok(Value::List(readable(new_list)))
}

fn prim_map(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let items = eval.force_list(&args[1], pos)?.to_vec();

  let mut new_list = vec![];

  for item in items {
    new_list.push(writable(Value::Apply(args[0].clone(), item)));
  }

  Ok(Value::List(readable(new_list)))
}

fn prim_add_error_context(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  match eval.clone_value(&args[1], pos) {
    Ok(v) => Ok(v),
    Err(e) => {
      let context = eval.coerce_new_string(pos, &args[0], CoerceOpts::default())?;
      Err(e.context(context.s))
    }
  }
}

fn prim_function_args(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let arg0 = eval.force_value(&args[0], pos)?;
  let (_, lambda) = arg0
    .as_lambda()
    .ok_or_else(|| err!(pos, "`functionArgs' requires a function"))?;
  match &lambda.arg {
    LambdaArg::Plain(_) => Ok(Value::Attrs(readable(Attrs::new()))),
    LambdaArg::Formals { formals, .. } => {
      let mut attrs = Attrs::new();
      for f in &formals.formals {
        attrs.insert(
          f.name.clone(),
          Located {
            v: writable(Value::Bool(f.def.is_some())),
            pos,
          },
        );
      }
      Ok(Value::Attrs(readable(attrs)))
    }
  }
}

fn prim_gen_list(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let len = eval.force_int(&args[1], pos)?;
  if len < 0 {
    throw!(pos, "cannot create a list of size {}", len);
  }
  let mut newlist = Vec::with_capacity(len as usize);
  for i in 0..len {
    newlist.push(writable(Value::Apply(
      args[0].clone(),
      writable(Value::Int(i)),
    )));
  }
  Ok(Value::List(readable(newlist)))
}

fn prim_map_attrs(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let attrs = eval.force_attrs(&args[1], pos)?;

  let mut new_attrs = Attrs::new();

  for (k, v) in &*attrs {
    new_attrs.insert(
      k.clone(),
      Located {
        pos,
        v: writable(Value::Apply(
          writable(Value::Apply(
            args[0].clone(),
            writable(Value::String(Str {
              s: k.to_string(),
              context: Default::default(),
            })),
          )),
          v.v.clone(),
        )),
      },
    );
  }

  Ok(Value::Attrs(readable(new_attrs)))
}

fn prim_attr_names(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut names = vec![];

  for k in eval.force_attrs(&args[0], pos)?.keys() {
    names.push(writable(Value::String(Str {
      s: k.to_string(),
      context: Default::default(),
    })));
  }

  Ok(Value::List(readable(names)))
}

fn prim_unsafe_discard_string_context(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  Ok(Value::String(Str {
    s: eval
      .coerce_new_string(pos, &args[0], CoerceOpts::default())?
      .s,
    context: Default::default(),
  }))
}

fn prim_concat_lists(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let items = eval.force_list(&args[0], pos)?;

  let mut new_list = vec![];

  for item in &*items {
    new_list.extend(eval.force_list(&item, pos)?.iter().cloned());
  }

  Ok(Value::List(readable(new_list)))
}

fn prim_generic_closure(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let attrs = eval.force_attrs(&args[0], pos)?;

  let s = attrs
    .get(&Ident::from("startSet"))
    .ok_or_else(|| err!(pos, "attribute `startSet` required"))?;

  let op = attrs
    .get(&Ident::from("operator"))
    .ok_or_else(|| err!(pos, "attribute `operator' required"))?;

  let start_set = eval.force_list(&s.v, pos)?;

  let mut work_set = vec![];

  for item in &*start_set {
    work_set.push(item.clone());
  }

  let mut res = vec![];
  let mut done_keys = BTreeSet::new();

  while let Some(ws) = work_set.pop() {
    let e = eval.force_attrs(&ws, pos)?.clone();
    if let Some(key) = e.get(&Ident::from("key")) {
      let key = eval.clone_value(&key.v, key.pos)?;
      if !done_keys.insert(CompareValues(Cow::Owned(key))) {
        continue;
      }
      res.push(ws.clone());
      let v = eval.call_function(&op.v, &ws, pos)?;
      let new_values = v
        .as_list()
        .ok_or_else(|| err!(pos, "`operator' should return a list, not {}", v.typename()))?;

      for item in new_values.iter() {
        work_set.push(item.clone());
      }
    }
  }

  Ok(Value::List(readable(res)))
}

fn prim_elem(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut exists = false;

  for item in eval.force_list(&args[1], pos)?.iter() {
    if eval.values_equal(&args[0], &item, pos)? {
      exists = true;
      break;
    }
  }

  Ok(Value::Bool(exists))
}

fn prim_seq(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let _ = eval.force_value(&args[0], pos)?;
  eval.clone_value(&args[1], pos)
}

fn prim_tail(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut val = eval.force_list(&args[0], pos)?.to_vec();
  if val.is_empty() {
    throw!(pos, "`tail' called on an empty list");
  }
  let val = val.split_off(1);
  Ok(Value::List(readable(val)))
}

fn prim_head(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let ix = 0;
  let list = eval.force_list(&args[0], pos)?;
  let val = list.get(ix as usize);
  match val {
    Some(x) => eval.clone_value(x, pos),
    None => throw!(pos, "list index {} out of bounds", ix),
  }
}

fn prim_elem_at(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let ix = eval.force_int(&args[1], pos)?;
  let list = eval.force_list(&args[0], pos)?;
  let val = list.get(ix as usize);
  match val {
    Some(x) => eval.clone_value(x, pos),
    None => throw!(pos, "list index {} out of bounds", ix),
  }
}

fn prim_substring(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let start = eval.force_int(&args[0], pos)?;
  let len = eval.force_int(&args[1], pos)?;
  let mut s = eval.coerce_new_string(pos, &args[2], CoerceOpts::default())?;
  if let Ok(start) = start.try_into() {
    if start < s.s.len() {
      s.s = s.s.split_off(start);
      if let Ok(len) = len.try_into() {
        if len < s.s.len() {
          s.s.truncate(len);
        }
      }
    }
    Ok(Value::String(s))
  } else {
    throw!(pos, "negative start position in `substring'")
  }
}

fn prim_string_length(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  Ok(Value::Int(eval.force_string(&args[0], pos)?.s.len() as i64))
}

fn prim_length(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  Ok(Value::Int(eval.force_list(&args[0], pos)?.len() as i64))
}

fn prim_list_to_attrs(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let items = eval.force_list(&args[0], pos)?;

  let mut seen = HashSet::new();
  let mut new_attrs = Attrs::new();

  for item in &*items {
    let attrs = eval.force_attrs(&item, pos)?;
    let name = if let Some(n) = attrs.get(&ident!("name")).cloned() {
      Ident::from(&*eval.force_string_no_context(&n.v, pos)?)
    } else {
      throw!(pos, "`name' attribute missing in a call to `listToAttrs'")
    };

    if seen.insert(name.clone()) {
      let value = if let Some(v) = attrs.get(&ident!("value")).cloned() {
        v
      } else {
        throw!(pos, "`value' attribute missing in a call to `listToAttrs'")
      };
      new_attrs.insert(name, value);
    }
  }

  Ok(Value::Attrs(readable(new_attrs)))
}

fn prim_remove_attrs(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let attrs = eval.force_attrs(&args[0], pos)?;
  let to_remove = eval.force_list(&args[1], pos)?;

  let mut names = vec![];
  for item in &*to_remove {
    let name = eval.force_string_no_context(&item, pos)?;
    names.push(Ident::from(&*name));
  }

  let mut new_attrs = Attrs::new();

  for (k, v) in attrs.iter() {
    if !names.contains(k) {
      new_attrs.insert(k.clone(), v.clone());
    }
  }

  Ok(Value::Attrs(readable(new_attrs)))
}

fn prim_dir_of(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let str = eval.coerce_new_string(
    pos,
    &args[0],
    CoerceOpts {
      copy_to_store: false,
      coerce_more: false,
    },
  )?;
  let p = Path::new(&str.s);
  let parent = p.parent().unwrap_or_else(|| {
    if p.is_absolute() {
      Path::new("/")
    } else {
      Path::new(".")
    }
  });
  if args[0].read().as_path().is_some() {
    Ok(Value::Path(parent.to_path_buf()))
  } else {
    Ok(Value::String(Str {
      s: parent.display().to_string(),
      context: str.context,
    }))
  }
}

fn prim_basename_of(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut str = eval.coerce_new_string(
    pos,
    &args[0],
    CoerceOpts {
      copy_to_store: false,
      coerce_more: false,
    },
  )?;
  str.s = Path::new(&str.s)
    .file_name()
    .map_or_else(String::new, |p| p.to_string_lossy().to_string());
  Ok(Value::String(str))
}

fn prim_compare_versions(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let v1 = eval.force_string_no_context(&args[0], pos)?;
  let v2 = eval.force_string_no_context(&args[1], pos)?;
  Ok(Value::Int(match compare_versions(&*v1, &*v2) {
    cmp::Ordering::Less => -1,
    cmp::Ordering::Equal => 0,
    cmp::Ordering::Greater => 1,
  }))
}

fn prim_path_exists(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut ctx = PathSet::new();
  let path = eval.coerce_to_path(pos, &args[0], &mut ctx)?;
  Ok(Value::Bool(path.exists()))
}

fn prim_get_env(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  Ok(Value::String(Str {
    s: std::env::var(&*eval.force_string_no_context(&args[0], pos)?).unwrap_or_default(),
    context: Default::default(),
  }))
}

fn prim_intersect_attrs(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let attrs1 = eval.force_attrs(&args[0], pos)?;
  let mut attrs2 = eval.force_attrs(&args[1], pos)?.clone();

  attrs2.drain_filter(|k, _| !attrs1.contains_key(k));

  Ok(Value::Attrs(readable(attrs2)))
}

fn prim_to_string(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let s = eval.coerce_new_string(
    pos,
    &args[0],
    CoerceOpts {
      coerce_more: true,
      copy_to_store: false,
    },
  )?;
  Ok(Value::String(s))
}

fn prim_abort(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let msg = eval.coerce_new_string(pos, &args[0], CoerceOpts::default())?;
  bail!(
    "evaluation aborted with the following error message: '{}'",
    msg.s
  )
}

fn prim_throw(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let msg = eval.coerce_new_string(pos, &args[0], CoerceOpts::default())?;
  bail!(Catchable::ThrownError(pos, msg.s))
}

fn prim_try_eval(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut attrs = Attrs::new();

  let success = match eval.force_value(&args[0], pos) {
    Err(e) => {
      if e.downcast_ref::<Catchable>().is_some() {
        false
      } else {
        return Err(e);
      }
    }
    Ok(_) => true,
  };

  attrs.insert(
    Ident::from("success"),
    Located {
      pos,
      v: writable(Value::Bool(success)),
    },
  );
  attrs.insert(
    ident!("value"),
    Located {
      pos,
      v: args[0].clone(),
    },
  );
  Ok(Value::Attrs(readable(attrs)))
}

fn prim_find_file(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let entries = eval.force_list(&args[0], pos)?;
  let target = eval.force_string_no_context(&args[1], pos)?;
  let mut path_parts = Path::new(&*target).components();
  let search_key = path_parts
    .next()
    .expect("there must be at least one item in a filepath")
    .as_os_str();
  let has_children = path_parts.as_path().iter().next().is_some();
  let add_children = move |p: PathBuf| {
    if has_children {
      p.join(path_parts.as_path())
    } else {
      p
    }
  };

  for entry in &*entries {
    let attrs = eval.force_attrs(entry, pos)?;
    let path = attrs
      .get(&Ident::from("path"))
      .ok_or_else(|| err!(pos, "attribute `path' missing"))?;
    let path = eval.coerce_new_string(
      pos,
      &path.v,
      CoerceOpts {
        copy_to_store: false,
        coerce_more: false,
      },
    )?;
    let prefix = attrs
      .get(&Ident::from("prefix"))
      .ok_or_else(|| err!(pos, "attribute `prefix' missing"))?;
    let prefix = eval.force_string_no_context(&prefix.v, pos)?;

    if search_key == &*prefix {
      let full = add_children(path.s.into());
      if full.exists() {
        return Ok(Value::Path(full));
      }
    } else if prefix.is_empty() {
      if let Ok(iter) = std::fs::read_dir(&*path.s) {
        for next_item in iter {
          let next_item = next_item?;
          if next_item.file_name() == search_key {
            return Ok(Value::Path(add_children(next_item.path())));
          }
        }
      }
    }
  }

  bail!(Catchable::ThrownError(
    pos,
    format!(
      "entry `{}' is not in the Nix search path (try adding it using -I)",
      target
    )
  ))
}

fn mk_nix_path() -> Value {
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
    let mut entry_attr = Attrs::new();
    entry_attr.insert(
      "path".into(),
      not_located(writable(Value::string_bare(path))),
    );
    entry_attr.insert(
      "prefix".into(),
      not_located(writable(Value::string_bare(prefix))),
    );
    entries.push(writable(Value::Attrs(readable(entry_attr))));
  }
  Value::List(readable(entries))
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

fn prim_scoped_import(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let real_path = eval
    .coerce_new_string(pos, &args[1], CoerceOpts::default())?
    .s;
  if !Path::new(&real_path).is_absolute() {
    bail!("path {} does not represent an absolute path", real_path);
  }
  let new_env = eval.force_attrs(&args[0], pos)?;
  if new_env.is_empty() {
    drop(new_env);
    eval.eval_file(real_path)
  } else {
    bail!(
      "scopedImport {:?} {:?}",
      new_env.keys(),
      args[1].read().debug()
    );
  }
}

fn prim_to_json(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut ctx = PathSet::new();
  let mut s = vec![];
  let mut ser = Serializer::new(&mut s);

  serialize_json(eval, &mut ser, &mut ctx, &args[0], pos)?;

  Ok(Value::String(Str {
    s: String::from_utf8_lossy(&s).to_string(),
    context: ctx,
  }))
}

fn serialize_json(
  eval: &Eval,
  ser: &mut Serializer<&mut Vec<u8>>,
  ctx: &mut PathSet,
  v: &ValueRef,
  pos: Pos,
) -> Result<()> {
  use serde::ser::Serializer as _;

  match &*eval.force_value(v, pos)? {
    Value::Null => Ok(ser.serialize_none()?),
    Value::String(Str { s, context }) => {
      ctx.extend(context.iter().cloned());
      Ok(ser.serialize_str(s)?)
    }
    x => unimplemented!("{:?}", x.debug()),
  }
}
