use itertools::Either;

use super::{CoerceOpts, Eval};
use crate::{prelude::*, value::*};
use std::{
  cmp,
  collections::HashSet,
  path::{Path, PathBuf},
};

const NIX_VERSION: &str = "2.7";

impl Eval {
  pub fn create_base_env(&self) -> Result<()> {
    self.add_constant("builtins", writable(Value::Attrs(readable(Attrs::new()))))?;
    self.add_constant("true", writable(Value::Bool(true)))?;
    self.add_constant("false", writable(Value::Bool(false)))?;
    self.add_constant("null", writable(Value::Null))?;

    self.add_constant(
      "__nixVersion",
      writable(Value::String(Str {
        s: NIX_VERSION.into(),
        context: Default::default(),
      })),
    )?;

    self.add_primop("throw", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let msg = eval.coerce_new_string(pos, &mut args[0].write(), CoerceOpts::default())?;
      bail!("thrown error: {}", msg.s);
    })?;

    let scoped_import = self.add_primop(
      "scopedImport",
      2,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        let real_path = eval
          .coerce_new_string(pos, &mut args[1].write(), CoerceOpts::default())?
          .s;
        if !Path::new(&real_path).is_absolute() {
          bail!("path {} does not represent an absolute path", real_path);
        }
        let mut arg0 = args[0].write();
        let new_env = eval.force_attrs(&mut arg0, pos)?;
        if new_env.is_empty() {
          drop(arg0);
          eval.eval_file(real_path)
        } else {
          bail!(
            "scopedImport {:?} {:?}",
            arg0.debug(),
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
      let msg = eval.coerce_new_string(pos, &mut args[0].write(), CoerceOpts::default())?;
      bail!(
        "evaluation aborted with the following error message: '{}'",
        msg.s
      )
    })?;

    self.add_primop("toString", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let s = eval.coerce_new_string(
        pos,
        &mut args[0].write(),
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
        let mut v1 = args[0].write();
        let mut v2 = args[1].write();

        let attrs1 = eval.force_attrs(&mut v1, pos)?;
        let mut attrs2 = eval.force_attrs(&mut v2, pos)?.clone();
        drop(v2);

        attrs2.drain_filter(|k, _| !attrs1.contains_key(k));

        Ok(Value::Attrs(readable(attrs2)))
      },
    )?;

    self.add_primop("__getEnv", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      Ok(Value::String(Str {
        s: std::env::var(eval.force_string_no_context(&mut args[0].write(), pos)?)
          .unwrap_or_default(),
        context: Default::default(),
      }))
    })?;

    self.add_primop(
      "__pathExists",
      1,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        let mut ctx = PathSet::new();
        let path = eval.coerce_to_path(pos, &mut args[0].write(), &mut ctx)?;
        Ok(Value::Bool(path.exists()))
      },
    )?;

    self.add_primop(
      "__compareVersions",
      2,
      |eval: &Self, pos, args: Vec<ValueRef>| {
        let mut v1 = args[0].write();
        let mut v2 = args[1].write();
        let v1 = eval.force_string_no_context(&mut v1, pos)?;
        let v2 = eval.force_string_no_context(&mut v2, pos)?;
        Ok(Value::Int(match compare_versions(v1, v2) {
          cmp::Ordering::Less => -1,
          cmp::Ordering::Equal => 0,
          cmp::Ordering::Greater => 1,
        }))
      },
    )?;

    self.add_primop("baseNameOf", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut str = eval.coerce_new_string(
        pos,
        &mut args[0].write(),
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
        &mut args[0].write(),
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
      let mut v1 = args[0].write();
      let mut v2 = args[1].write();
      let attrs = eval.force_attrs(&mut v1, pos)?;
      let to_remove = eval.force_list(&mut v2, pos)?;

      let mut names = vec![];
      for item in to_remove {
        let mut item_ = item.write();
        let name = eval.force_string_no_context(&mut item_, pos)?;
        names.push(Ident::from(name));
      }

      let mut new_attrs = Attrs::new();

      for (k, v) in attrs {
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
        let mut v1 = args[0].write();
        let items = eval.force_list(&mut v1, pos)?;

        let mut seen = HashSet::new();
        let mut new_attrs = Attrs::new();

        for item in items {
          let mut item = item.write();
          let attrs = eval.force_attrs(&mut item, pos)?;
          let name = if let Some(n) = attrs.get(&ident!("name")).cloned() {
            Ident::from(eval.force_string_no_context(&mut n.v.write(), pos)?)
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
      Ok(Value::Int(
        eval.force_list(&mut args[0].write(), pos)?.len() as i64,
      ))
    })?;

    self.add_primop("__attrNames", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut names = vec![];

      for k in eval.force_attrs(&mut args[0].write(), pos)?.keys() {
        names.push(writable(Value::String(Str {
          s: k.to_string(),
          context: Default::default(),
        })));
      }

      Ok(Value::List(readable(names)))
    })?;

    self.add_primop("__mapAttrs", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut v1 = args[1].write();
      let attrs = eval.force_attrs(&mut v1, pos)?;

      let mut new_attrs = Attrs::new();

      for (k, v) in attrs {
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
      let len = eval.force_int(&mut args[1].write(), pos)?;
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
      let mut v1 = args[1].write();
      let items = eval.force_list(&mut v1, pos)?;

      let mut new_list = vec![];

      for item in items {
        new_list.push(writable(Value::Apply(args[0].clone(), item.clone())));
      }

      Ok(Value::List(readable(new_list)))
    })?;

    self.add_primop("__filter", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut functor = args[0].write();
      let mut v1 = args[1].write();
      let items = eval.force_list(&mut v1, pos)?;

      let mut new_list = vec![];

      for item in items {
        let result = match eval.call_function(&mut functor, item, pos)? {
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

      if new_list.len() == items.len() {
        Ok(v1.clone())
      } else {
        Ok(Value::List(readable(new_list)))
      }
    })?;

    self.add_primop("__lessThan", 2, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut v1 = args[0].write();
      let mut v2 = args[1].write();

      eval.force_value(&mut v1, pos)?;
      eval.force_value(&mut v2, pos)?;

      if let Some(res) = less_than(&v1, &v2) {
        Ok(Value::Bool(res))
      } else {
        bail!("cannot compare {} with {}", v1.typename(), v2.typename())
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

    Ok(())
  }

  pub(super) fn coerce_new_string(
    &self,
    pos: Pos,
    value: &mut Value,
    opts: CoerceOpts,
  ) -> Result<Str> {
    let mut ctx = PathSet::new();
    let s = self.coerce_to_string(pos, value, &mut ctx, opts)?;
    Ok(Str { s, context: ctx })
  }

  pub(super) fn coerce_to_path(
    &self,
    pos: Pos,
    value: &mut Value,
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
    value: &mut Value,
    context: &mut PathSet,
    opts: CoerceOpts,
  ) -> Result<String> {
    self.force_value(value, pos)?;

    Ok(match value {
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
          return self.coerce_to_string(pos, &mut x.v.write(), context, opts);
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
          s.push_str(
            self
              .coerce_to_string(pos, &mut item.write(), context, opts)?
              .as_str(),
          );
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

fn less_than(lhs: &Value, rhs: &Value) -> Option<bool> {
  if let Some(pair) = super::numbers(lhs, rhs) {
    Some(match pair {
      Either::Left((a, b)) => a < b,
      Either::Right((a, b)) => a < b,
    })
  } else {
    match (lhs, rhs) {
      (Value::String(Str { s: s1, .. }), Value::String(Str { s: s2, .. })) => Some(s1 < s2),
      (Value::Path(p1), Value::Path(p2)) => Some(p1 < p2),
      _ => None,
    }
  }
}
