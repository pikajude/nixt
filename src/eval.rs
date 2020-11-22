use crate::{prelude::*, syntax::expr::*, value::*};
use parking_lot::Mutex;
use std::{
  collections::HashMap,
  path::{Path, PathBuf},
  sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct CoerceOpts {
  coerce_more: bool,
  copy_to_store: bool,
}

impl Default for CoerceOpts {
  fn default() -> Self {
    Self {
      coerce_more: false,
      copy_to_store: true,
    }
  }
}

#[derive(Deref, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
struct CountCalls(bool);

impl Default for CountCalls {
  fn default() -> Self {
    Self(std::env::var("NIX_COUNT_CALLS").map_or(false, |x| x != "0"))
  }
}

#[derive(Default)]
pub struct Eval {
  selects: Writable<HashMap<Pos, usize>>,
  count_calls: bool,
  static_base_env: StaticEnvRef,
  base_env: EnvRef,
  base_env_displacement: AtomicUsize,

  file_parse_cache: Mutex<HashMap<PathBuf, ExprRef>>,
  file_eval_cache: Mutex<HashMap<PathBuf, ValueRef>>,

  number_avoided: AtomicUsize,
  // number_function_calls: AtomicUsize,
}

impl Eval {
  pub fn create_base_env(&self) -> Result<()> {
    self.add_constant(
      "builtins",
      writable(Value::Attrs(writable(HashMap::with_capacity(128)))),
    )?;
    self.add_constant("true", writable(Value::Bool(true)))?;
    self.add_constant("false", writable(Value::Bool(false)))?;
    self.add_constant("null", writable(Value::Null))?;

    self.add_primop("throw", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut ctx = PathSet::new();
      let msg =
        eval.coerce_to_string(pos, &mut args[0].write(), &mut ctx, CoerceOpts::default())?;
      bail!("thrown error: {}", msg);
    })?;

    let scoped_import =
      self.add_primop("scopedImport", 2, |_: &Self, pos, args: Vec<ValueRef>| {
        bail!("scopedImport: {:?}, {:?}", pos, args);
      })?;
    let mut unscoped_import =
      Value::Apply(scoped_import, writable(Value::Attrs(Default::default())));
    self.force_value(&mut unscoped_import, Pos::none())?;
    self.add_constant("import", writable(unscoped_import))?;

    self.add_primop("abort", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut ctx = PathSet::new();
      let msg =
        eval.coerce_to_string(pos, &mut args[0].write(), &mut ctx, CoerceOpts::default())?;
      bail!(
        "evaluation aborted with the following error message: '{}'",
        msg
      )
    })?;

    self.add_primop("toString", 1, |eval: &Self, pos, args: Vec<ValueRef>| {
      let mut ctx = PathSet::new();
      let msg = eval.coerce_to_string(
        pos,
        &mut args[0].write(),
        &mut ctx,
        CoerceOpts {
          coerce_more: true,
          copy_to_store: false,
        },
      )?;
      Ok(Value::String(Str {
        context: ctx,
        s: msg,
      }))
    })?;

    Ok(())
  }

  pub fn eval(&self, expr: &Expr, env: &EnvRef) -> Result<Value> {
    trace!("eval {}", expr);
    match expr {
      Expr::Var(n) => {
        let v2 = self
          .lookup_var(env.clone(), &*n.read(), false)?
          .expect("internal error");
        let x = v2.read();
        Ok(x.clone())
      }
      Expr::Let { attrs, body } => {
        let mut new_env = Env::default();
        new_env.up = Some(env.clone());
        let new_env = writable(new_env);
        for v in attrs.attrs.values() {
          let thunk = self.maybe_thunk(
            v.rhs.clone(),
            if v.inherited {
              env.clone()
            } else {
              new_env.clone()
            },
          )?;
          new_env.write().values.push(thunk);
        }
        self.eval(&*body, &new_env)
      }
      Expr::Apply { pos, lhs, rhs } => {
        let vfun = self.eval(&*lhs, env)?;
        let arg = self.maybe_thunk(rhs.clone(), env.clone())?;
        self.call_function(vfun, &arg, *pos)
      }
      Expr::If {
        cond, rhs1, rhs2, ..
      } => {
        if self.eval_bool(env, &*cond)? {
          self.eval(&*rhs1, env)
        } else {
          self.eval(&*rhs2, env)
        }
      }
      Expr::HasAttr { lhs, path } => {
        let mut vtmp = self.eval(&*lhs, env)?;

        for item in path {
          self.force_value(&mut vtmp, Pos::none())?;
          let name = self.get_name(item, env)?;
          let vguard = vtmp;
          if let Some(attrs) = vguard.as_attrs() {
            let vguard2 = attrs.read();
            if let Some(subattrs) = vguard2.get(&name) {
              let tmp_value = subattrs.v.read().clone();
              drop(vguard2);
              drop(vguard);
              vtmp = tmp_value;
              continue;
            }
          }
          return Ok(Value::Bool(false));
        }

        Ok(Value::Bool(true))
      }
      Expr::Op {
        bin: Bin::Or,
        lhs,
        rhs,
        ..
      } => Ok(Value::Bool(
        self.eval_bool(env, &*lhs)? || self.eval_bool(env, &*rhs)?,
      )),
      Expr::Op {
        bin: Bin::Impl,
        lhs,
        rhs,
        ..
      } => Ok(Value::Bool(
        !self.eval_bool(env, &*lhs)? || self.eval_bool(env, &*rhs)?,
      )),
      Expr::Not(e) => Ok(Value::Bool(!self.eval_bool(env, &*e)?)),
      Expr::ConcatStrings {
        pos,
        force_string,
        parts,
      } => self.eval_concat(*pos, *force_string, parts, env),
      Expr::String { s } => Ok(Value::String(Str {
        s: s.clone(),
        context: Default::default(),
      })),
      x => unimplemented!("{:?}", x),
    }
  }

  fn eval_concat(
    &self,
    pos: Pos,
    force_string: bool,
    parts: &[Expr],
    env: &EnvRef,
  ) -> Result<Value> {
    enum ConcatTy {
      Int(i64),
      Float(f64),
      Path(PathBuf),
      String(String),
    }

    let mut ctx = PathSet::new();

    let (part0, partrest) = parts
      .split_first()
      .expect("empty list inside ConcatStrings");

    let part0 = self.eval(part0, env)?;

    let should_copy = force_string || part0.as_string().is_some();

    let first = match part0 {
      Value::Int(i) => ConcatTy::Int(i),
      Value::Float(f) => ConcatTy::Float(f),
      Value::Path(p) => ConcatTy::Path(p),
      mut x => ConcatTy::String(self.coerce_to_string(
        pos,
        &mut x,
        &mut ctx,
        CoerceOpts {
          coerce_more: false,
          copy_to_store: should_copy,
        },
      )?),
    };

    let final_ = partrest.iter().try_fold(first, |vtmp, part| {
      let part = self.eval(part, env)?;
      Ok(match (vtmp, part) {
        (ConcatTy::Int(i0), Value::Int(i1)) => ConcatTy::Int(i0 + i1),
        (ConcatTy::Int(i0), Value::Float(i1)) => ConcatTy::Float((i0 as f64) + i1),
        (ConcatTy::Int(_), x) => bail!("cannot add {} to an integer", x.typename()),
        (ConcatTy::Float(f0), Value::Int(f1)) => ConcatTy::Float(f0 + (f1 as f64)),
        (ConcatTy::Float(f0), Value::Float(f1)) => ConcatTy::Float(f0 + f1),
        (ConcatTy::Float(_), x) => bail!("cannot add {} to a float", x.typename()),
        (ConcatTy::Path(p), mut x) => {
          let s = self.coerce_to_string(
            pos,
            &mut x,
            &mut ctx,
            CoerceOpts {
              coerce_more: false,
              copy_to_store: should_copy,
            },
          )?;
          if !ctx.is_empty() {
            bail!("a string that refers to a store path cannot be appended to a path");
          }
          ConcatTy::Path(p.join(s.strip_prefix('/').unwrap_or(&s)))
        }
        (ConcatTy::String(mut s0), mut x) => {
          let s = self.coerce_to_string(
            pos,
            &mut x,
            &mut ctx,
            CoerceOpts {
              coerce_more: false,
              copy_to_store: should_copy,
            },
          )?;
          s0.push_str(&s);
          ConcatTy::String(s0)
        }
      })
    })?;

    Ok(match final_ {
      ConcatTy::Int(i) => Value::Int(i),
      ConcatTy::Float(f) => Value::Float(f),
      ConcatTy::Path(p) => Value::Path(p),
      ConcatTy::String(s) => Value::String(Str { s, context: ctx }),
    })
  }

  pub fn eval_file<P: AsRef<Path>>(&self, path: P) -> Result<Value> {
    let path = path.as_ref();

    let mut fec = self.file_eval_cache.lock();

    if let Some(v2) = fec.get(path) {
      return Ok(v2.read().clone());
    }

    let mut fpc = self.file_parse_cache.lock();

    let e = match fpc.get(path) {
      None => {
        let parsed = crate::syntax::parse_from_file(path, &self.static_base_env)?;
        let e = readable(parsed);
        fpc.insert(path.to_path_buf(), e.clone());
        e
      }
      Some(x) => x.clone(),
    };

    let v = self.eval(&*e, &self.base_env)?;

    fec.insert(path.to_path_buf(), writable(v.clone()));

    Ok(v)
  }

  pub fn eval_inline<S: AsRef<str>>(&self, input: S) -> Result<Value> {
    let parsed = crate::syntax::parse_inline(input.as_ref(), &self.static_base_env)?;

    self.eval(&parsed, &self.base_env)
  }

  fn maybe_thunk(&self, e: ExprRef, env: EnvRef) -> Result<ValueRef> {
    match &*e {
      Expr::Var(v) => {
        let vread = v.read();
        if let Some(x) = self.lookup_var(env.clone(), &*vread, true)? {
          Ok(x)
        } else {
          drop(vread);
          Ok(writable(Value::Thunk(env, e)))
        }
      }
      _ => Ok(writable(Value::Thunk(env, e))),
    }
  }

  fn force_value(&self, m: &mut Value, pos: Pos) -> Result<()> {
    match m {
      Value::Thunk(env, expr) => {
        let result = self.eval(&*expr, &env)?;
        *m = result;
      }
      Value::Apply(lhs, rhs) => {
        let lread = lhs.read().clone();
        *m = self.call_function(lread, &rhs, pos)?;
      }
      _ => {}
    }
    Ok(())
  }

  fn get_name(&self, attr: &AttrName, env: &EnvRef) -> Result<Ident> {
    match attr {
      AttrName::Static(s) => Ok(s.clone()),
      AttrName::Dynamic(e) => {
        let mut v = self.eval(&*e, env)?;
        let name_str = self.force_string_no_context(&mut v, Pos::none())?;
        Ok(Ident::from(name_str))
      }
    }
  }

  fn call_function(&self, mut fun: Value, arg: &ValueRef, pos: Pos) -> Result<Value> {
    trace!("calling {}", fun.typename());
    self.force_value(&mut fun, pos)?;

    match fun {
      Value::Attrs(ref a) => {
        let aread = a.read();
        if let Some(functor) = aread.get(&ident!("__functor")) {
          let functor = functor.v.read().clone();
          drop(aread);
          let v2 = self.call_function(functor, &writable(fun), pos)?;
          self.call_function(v2, arg, pos)
        } else {
          bail!("cannot call a value of type attrset as a function")
        }
      }
      Value::Primop(p, mut args) => {
        args.push(arg.clone());
        trace!("{} {}", p.arity, args.len());
        if p.arity as usize == args.len() {
          (p.fun)(self, pos, args)
        } else {
          Ok(Value::Primop(p, args))
        }
      }
      Value::Lambda(_, _) => todo!("lambda"),
      v => bail!("cannot call a value of type {} as a function", v.typename()),
    }
  }

  fn lookup_var(&self, mut env: EnvRef, var: &Var, no_eval: bool) -> Result<Option<ValueRef>> {
    trace!("resolving {}", var.name);
    match var.displ.level() {
      Some(l) => {
        env = Env::climb(env, l as u8);
      }
      None => bail!("unresolved: {}", var.name),
    }

    if let Displacement::Static { offset, level } = var.displ {
      trace!(
        "returning a reference to item {} @ {} within {:?}",
        offset,
        level,
        env.read().values
      );
      return Ok(Some(env.read().values[offset].clone()));
    }

    loop {
      let mut env_ = env.write();
      if env_.env_type == EnvType::HasWithExpr {
        if no_eval {
          return Ok(None);
        }
        env_.values[0] = writable(Value::Bool(false));
        env_.env_type = EnvType::HasWithAttrs;
      }
      if let Some(j) = env_.values[0].read().as_attrs() {
        if let Some(j) = j.read().get(&var.name) {
          if self.count_calls {
            self
              .selects
              .write()
              .entry(j.pos)
              .and_modify(|e| *e += 1)
              .or_default();
          }
          return Ok(Some(j.v.clone()));
        }
      }
      let prev_with = env_.prev_with;
      drop(env_);
      match prev_with {
        None => bail!("undefined variable '{}' at {:?}", var.name, var.pos),
        Some(p) => env = Env::climb(env, p),
      }
    }
  }

  fn add_constant(&self, name: &str, v: ValueRef) -> Result<ValueRef> {
    let displ = self.base_env_displacement.fetch_add(1, Ordering::Acquire);
    self
      .static_base_env
      .write()
      .vars
      .insert(Ident::from(name), displ);
    let mut be = self.base_env.write();
    be.values.push(v.clone());
    if let Some(name2) = name.strip_prefix("__") {
      be.values[0]
        .read()
        .as_attrs()
        .unwrap()
        .write()
        .insert(Ident::from(name2), not_located(v.clone()));
    }
    Ok(v)
  }

  fn add_primop<P: PrimopFn + 'static>(&self, name: &str, arity: u8, fun: P) -> Result<ValueRef> {
    let sym = Ident::from(name.strip_prefix("__").unwrap_or(name));

    if arity == 0 {}

    let v = writable(Value::Primop(
      Primop {
        fun: readable(fun),
        name: sym.clone(),
        arity,
      },
      vec![],
    ));

    let displ = self.base_env_displacement.fetch_add(1, Ordering::Acquire);
    let mut be = self.base_env.write();
    self
      .static_base_env
      .write()
      .vars
      .insert(Ident::from(name), displ);
    be.values.insert(displ, v.clone());
    be.values[0]
      .read()
      .as_attrs()
      .expect("builtins is gone 🦀")
      .write()
      .insert(sym, not_located(v.clone()));

    Ok(v)
  }

  fn eval_bool(&self, env: &EnvRef, e: &Expr) -> Result<bool> {
    match self.eval(e, env)? {
      Value::Bool(b) => Ok(b),
      q => bail!("expected bool, got {:?}", q),
    }
  }

  fn force_string<'v>(&self, v: &'v mut Value, pos: Pos) -> Result<&'v Str> {
    self.force_value(v, pos)?;
    v.as_string()
      .ok_or_else(|| anyhow!("expected string, got {}", v.typename()))
  }

  fn force_string_no_context<'v>(&self, v: &'v mut Value, pos: Pos) -> Result<&'v str> {
    let guard = self.force_string(v, pos)?;
    if guard.context.is_empty() {
      Ok(guard.s.as_str())
    } else {
      bail!(
        "the string '{}' is not allowed to refer to a store path, such as '{}'",
        guard.s,
        guard.context.first().unwrap()
      );
    }
  }

  fn coerce_to_string(
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
          bail!("must copy to store")
        } else {
          p.display().to_string()
        }
      }
      Value::Attrs(a) => {
        if let Some(x) = a.read().get(&ident!("outPath")) {
          return self.coerce_to_string(pos, &mut x.v.write(), context, opts);
        } else {
          bail!("cannot coerce a set to a string")
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
      v => bail!("cannot coerce {} to a string", v.typename()),
    })
  }
}

#[test]
fn test_nixpkgs_eval() -> Result<()> {
  crate::logger::init()?;

  let e = Eval::default();
  e.create_base_env()?;

  eprintln!(
    "{:?}",
    e.eval_file("/home/jude/.code/nix/pkgs/default.nix")
      .or_exit()
  );
  Ok(())
}

#[test]
fn test_attrs_equality() {
  let e = Eval::default();
  e.create_base_env().or_exit();

  macro_rules! assert_true {
    ($expr:literal) => {
      let v = e.eval_inline($expr).or_exit();
      match v {
        Value::Bool(b) => assert_eq!(b, true),
        v => panic!("unexpected value {}", v.typename()),
      }
    };
  };
  macro_rules! assert_false {
    ($($t:tt)+) => {};
  };

  assert_true!("let x = { f = _: 1; }; in x == x");
  assert_false!("let x = { f = _: 1; }; in x == { inherit (x) f; }");
  assert_true!("let x.f = { y = _: 1; }; in x == { inherit (x) f; }");

  assert_false!("let x = { f = _: 1; }; in x == x // { inherit (x) f; }");
  assert_true!("let x = { f = _: 1; }; in x == { inherit (x) f; } // x");
  assert_true!(r#"let x = { f = _: 1; }; in x == builtins.removeAttrs (x // { g = 1; }) ["g"]"#);
}
