use super::{CoerceOpts, Eval};
use crate::{prelude::*, value::*};
use itertools::Either;
use parking_lot::Mutex;
use regex::Regex;
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

    self.add_primop("throw", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let msg = eval.coerce_new_string(pos, &args[0], CoerceOpts::default())?;
      bail!("thrown error: {}", msg.s);
    })?;

    let scoped_import = self.add_primop(
      "scopedImport",
      2,
      |eval: &Self, pos, args: Vec<ValueRef>| {
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
      },
    )?;
    let mut unscoped_import =
      Value::Apply(scoped_import, writable(Value::Attrs(Default::default())));
    self.force_value(&mut unscoped_import, Pos::none())?;
    self.add_constant("import", writable(unscoped_import))?;

    self.add_primop("abort", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let msg = eval.coerce_new_string(pos, &args[0], CoerceOpts::default())?;
      bail!(
        "evaluation aborted with the following error message: '{}'",
        msg.s
      )
    })?;

    self.add_primop("toString", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let s = eval.coerce_new_string(
        pos,
        &args[0],
        CoerceOpts {
          coerce_more: true,
          copy_to_store: false,
        },
      )?;
      Ok(Value::String(s))
    })?;

    self.add_primop("__isString", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      Ok(Value::Bool(
        eval
          .force_value(&mut args[0].write(), pos)?
          .as_string()
          .is_some(),
      ))
    })?;

    self.add_primop("__isList", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      Ok(Value::Bool(
        eval
          .force_value(&mut args[0].write(), pos)?
          .as_list()
          .is_some(),
      ))
    })?;

    self.add_primop("__isAttrs", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      Ok(Value::Bool(
        eval
          .force_value(&mut args[0].write(), pos)?
          .as_attrs()
          .is_some(),
      ))
    })?;

    self.add_primop(
      "__isFunction",
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        Ok(Value::Bool(matches!(
          eval.force_value(&mut args[0].write(), pos)?,
          Value::Primop { .. } | Value::Lambda { .. },
        )))
      },
    )?;

    self.add_primop(
      "__intersectAttrs",
      2,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        let attrs1 = eval.force_attrs(&args[0], pos)?;
        let mut attrs2 = eval.force_attrs(&args[1], pos)?.clone();

        attrs2.drain_filter(|k, _| !attrs1.contains_key(k));

        Ok(Value::Attrs(readable(attrs2)))
      },
    )?;

    self.add_primop("__getEnv", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      Ok(Value::String(Str {
        s: std::env::var(&*eval.force_string_no_context(&args[0], pos)?).unwrap_or_default(),
        context: Default::default(),
      }))
    })?;

    self.add_primop(
      "__pathExists",
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        let mut ctx = PathSet::new();
        let path = eval.coerce_to_path(pos, &args[0], &mut ctx)?;
        Ok(Value::Bool(path.exists()))
      },
    )?;

    self.add_primop(
      "__compareVersions",
      2,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        let v1 = eval.force_string_no_context(&args[0], pos)?;
        let v2 = eval.force_string_no_context(&args[1], pos)?;
        Ok(Value::Int(match compare_versions(&*v1, &*v2) {
          cmp::Ordering::Less => -1,
          cmp::Ordering::Equal => 0,
          cmp::Ordering::Greater => 1,
        }))
      },
    )?;

    self.add_primop("baseNameOf", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
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
    })?;

    self.add_primop("dirOf", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
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
    })?;

    self.add_primop("removeAttrs", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
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
    })?;

    self.add_primop(
      "__listToAttrs",
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
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
      },
    )?;

    self.add_primop("__length", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      Ok(Value::Int(eval.force_list(&args[0], pos)?.len() as i64))
    })?;

    self.add_primop(
      "__stringLength",
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        Ok(Value::Int(eval.force_string(&args[0], pos)?.s.len() as i64))
      },
    )?;

    self.add_primop("__substring", 3, |eval: &Self, pos, args: Vec<ValueRef>| {
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
    })?;

    self.add_primop("__elemAt", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let ix = eval.force_int(&args[1], pos)?;
      let list = eval.force_list(&args[0], pos)?;
      let val = list.get(ix as usize);
      match val {
        Some(x) => eval.clone_value(x, pos),
        None => throw!(pos, "list index {} out of bounds", ix),
      }
    })?;

    self.add_primop("__head", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let ix = 0;
      let list = eval.force_list(&args[0], pos)?;
      let val = list.get(ix as usize);
      match val {
        Some(x) => eval.clone_value(x, pos),
        None => throw!(pos, "list index {} out of bounds", ix),
      }
    })?;

    self.add_primop("__tail", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut val = eval.force_list(&args[0], pos)?.to_vec();
      if val.is_empty() {
        throw!(pos, "`tail' called on an empty list");
      }
      let val = val.split_off(1);
      Ok(Value::List(readable(val)))
    })?;

    self.add_primop("__seq", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      eval.force_value(&mut args[0].write(), pos)?;
      eval.clone_value(&args[1], pos)
    })?;

    self.add_primop("__elem", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut v1 = args[0].write();
      let mut exists = false;

      for item in eval.force_list(&args[1], pos)?.iter() {
        if eval.values_equal(&mut v1, &mut item.write(), pos)? {
          exists = true;
          break;
        }
      }

      Ok(Value::Bool(exists))
    })?;

    self.add_primop(
      "__genericClosure",
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
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

        let mut op = op.v.write();

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
            let v = eval.call_function(&mut op, &ws, pos)?;
            let new_values = v
              .as_list()
              .ok_or_else(|| err!(pos, "`operator' should return a list, not {}", v.typename()))?;

            for item in new_values.iter() {
              work_set.push(item.clone());
            }
          }
        }

        Ok(Value::List(readable(res)))
      },
    )?;

    self.add_primop(
      "__concatLists",
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        let items = eval.force_list(&args[0], pos)?;

        let mut new_list = vec![];

        for item in &*items {
          new_list.extend(eval.force_list(&item, pos)?.iter().cloned());
        }

        Ok(Value::List(readable(new_list)))
      },
    )?;

    self.add_primop(
      "__unsafeDiscardStringContext",
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        Ok(Value::String(Str {
          s: eval
            .coerce_new_string(pos, &args[0], CoerceOpts::default())?
            .s,
          context: Default::default(),
        }))
      },
    )?;

    self.add_primop("__attrNames", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut names = vec![];

      for k in eval.force_attrs(&args[0], pos)?.keys() {
        names.push(writable(Value::String(Str {
          s: k.to_string(),
          context: Default::default(),
        })));
      }

      Ok(Value::List(readable(names)))
    })?;

    self.add_primop("__mapAttrs", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
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
    })?;

    self.add_primop("__genList", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
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
    })?;

    self.add_primop("map", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let items = eval.force_list(&args[1], pos)?.to_vec();

      let mut new_list = vec![];

      for item in items {
        new_list.push(writable(Value::Apply(args[0].clone(), item)));
      }

      Ok(Value::List(readable(new_list)))
    })?;

    self.add_primop("__filter", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut functor = args[0].write();
      // TODO: why does the lock on args[1] needs to be released before the filter fn
      // is called?
      let items = eval.force_list(&args[1], pos)?.to_vec();

      let mut new_list = vec![];

      for item in items {
        let result = match eval.call_function(&mut functor, &item, pos)? {
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
    })?;

    self.add_primop("__lessThan", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut v1 = args[0].write();
      let mut v2 = args[1].write();

      let v1 = CompareValues(Cow::Borrowed(eval.force_value(&mut v1, pos)?));
      let v2 = CompareValues(Cow::Borrowed(eval.force_value(&mut v2, pos)?));

      if let Some(res) = v1.partial_cmp(&v2) {
        Ok(Value::Bool(res == cmp::Ordering::Less))
      } else {
        bail!(
          "cannot compare {} with {}",
          v1.0.typename(),
          v2.0.typename()
        )
      }
    })?;

    self.add_primop("__sub", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut v1 = args[0].write();
      let mut v2 = args[1].write();

      eval.force_value(&mut v1, pos)?;
      eval.force_value(&mut v2, pos)?;

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
    })?;

    self.add_primop("__mul", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut v1 = args[0].write();
      let mut v2 = args[1].write();

      eval.force_value(&mut v1, pos)?;
      eval.force_value(&mut v2, pos)?;

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
    })?;

    self.add_primop("__div", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut v1 = args[0].write();
      let mut v2 = args[1].write();

      eval.force_value(&mut v1, pos)?;
      eval.force_value(&mut v2, pos)?;

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
    })?;

    self.add_primop("__split", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
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
    })?;

    self.add_primop(
      "__concatStringsSep",
      2,
      |eval: &Self, pos, args: Vec<ValueRef>| {
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
      },
    )?;

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
    self.force_value(&mut value.write(), pos)?;

    Ok(match &*value.read() {
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
