use crate::{
  prelude::*,
  syntax::expr::{self, *},
  value::{Attrs, *},
};
use itertools::Either;
use parking_lot::{
  MappedRwLockReadGuard, Mutex, RwLockReadGuard, RwLockUpgradableReadGuard, RwLockWriteGuard,
};
use std::{
  collections::HashMap,
  num::NonZeroU8,
  path::{Path, PathBuf},
  sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
  },
};

mod builtins;

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
  /* number_avoided: AtomicUsize,
   * number_function_calls: AtomicUsize, */
}

impl Eval {
  pub fn eval(&self, env: &EnvRef, expr: &Expr) -> Result<Value> {
    match expr {
      Expr::Var(n) => {
        let v2 = self
          .lookup_var(env.clone(), &*n.read(), false)?
          .expect("internal error");
        let x = Ok(self.force_value(&v2, Pos::none())?.clone());
        x
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
        self.eval(&new_env, &*body)
      }
      Expr::Apply { pos, lhs, rhs } => {
        let vfun = writable(self.eval(env, &*lhs)?);
        let arg = self.maybe_thunk(rhs.clone(), env.clone())?;
        self.call_function(&vfun, &arg, *pos)
      }
      Expr::If {
        cond, rhs1, rhs2, ..
      } => {
        if self.eval_bool(env, &*cond)? {
          self.eval(env, &*rhs1)
        } else {
          self.eval(env, &*rhs2)
        }
      }
      Expr::HasAttr { lhs, path } => {
        let mut vtmp = writable(self.eval(env, &*lhs)?);

        for item in path {
          let tmp2 = self.force_value(&vtmp, item.pos)?;
          let name = self.get_name(&item.v, env)?;
          if let Some(vguard2) = tmp2.as_attrs() {
            if let Some(subattrs) = vguard2.get(&name) {
              let n = subattrs.v.clone();
              drop(tmp2);
              vtmp = n;
              continue;
            }
          }
          return Ok(Value::Bool(false));
        }

        Ok(Value::Bool(true))
      }
      Expr::Op { bin, lhs, rhs, pos } => self.eval_operator(env, *bin, lhs, rhs, *pos),
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
      Expr::Path { path } => Ok(Value::Path(path.clone())),
      Expr::Int { n } => Ok(Value::Int(*n)),
      Expr::Attrs(a) => self.build_attrs(env.clone(), a),
      Expr::Select {
        pos,
        lhs,
        def,
        path,
      } => Ok(
        self
          .select(env, *pos, lhs, def.as_deref(), path)?
          .read()
          .clone(),
      ),
      Expr::With {
        env: with_env,
        body,
        prev_with,
        ..
      } => {
        let mut env2 = Env::default();
        env2.up = Some(env.clone());
        env2.env_type = EnvType::HasWithExpr;
        env2.prev_with = NonZeroU8::new(prev_with.load(Ordering::Relaxed) as u8);
        env2
          .values
          .push(self.maybe_thunk(with_env.clone(), env.clone())?);
        self.eval(&writable(env2), body)
      }
      Expr::Lambda(l) => Ok(Value::Lambda(env.clone(), l.clone())),
      Expr::Assert { pos, cond, body } => {
        if self.eval_bool(env, cond)? {
          self.eval(env, body)
        } else {
          throw!(*pos, "assertion failed")
        }
      }
      Expr::List(items) => {
        let mut new_list = vec![];
        for item in items {
          new_list.push(self.maybe_thunk(item.clone(), env.clone())?);
        }
        Ok(Value::List(readable(new_list)))
      }
      x => unimplemented!("{:?}", x),
    }
  }

  fn clone_value(&self, v: &ValueRef, pos: Pos) -> Result<Value> {
    Ok(self.force_value(&v, pos)?.clone())
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

    let part0 = self.eval(env, part0)?;

    let should_copy = force_string || part0.as_string().is_some();

    let first = match part0 {
      Value::Int(i) => ConcatTy::Int(i),
      Value::Float(f) => ConcatTy::Float(f),
      Value::Path(p) => ConcatTy::Path(p),
      v => ConcatTy::String(self.coerce_to_string(
        pos,
        &writable(v),
        &mut ctx,
        CoerceOpts {
          coerce_more: false,
          copy_to_store: should_copy,
        },
      )?),
    };

    let final_ = partrest.iter().try_fold(first, |vtmp, part| {
      let part = self.eval(env, part)?;
      Ok(match (vtmp, part) {
        (ConcatTy::Int(i0), Value::Int(i1)) => ConcatTy::Int(i0 + i1),
        (ConcatTy::Int(i0), Value::Float(i1)) => ConcatTy::Float((i0 as f64) + i1),
        (ConcatTy::Int(_), x) => throw!(pos, "cannot add {} to an integer", x.typename()),
        (ConcatTy::Float(f0), Value::Int(f1)) => ConcatTy::Float(f0 + (f1 as f64)),
        (ConcatTy::Float(f0), Value::Float(f1)) => ConcatTy::Float(f0 + f1),
        (ConcatTy::Float(_), x) => throw!(pos, "cannot add {} to a float", x.typename()),
        (ConcatTy::Path(p), x) => {
          let s = self.coerce_to_string(
            pos,
            &writable(x),
            &mut ctx,
            CoerceOpts {
              coerce_more: false,
              copy_to_store: should_copy,
            },
          )?;
          if !ctx.is_empty() {
            throw!(
              pos,
              "a string that refers to a store path cannot be appended to a path"
            );
          }
          ConcatTy::Path(p.join(s.strip_prefix('/').unwrap_or(&s)))
        }
        (ConcatTy::String(mut s0), x) => {
          let s = self.coerce_to_string(
            pos,
            &writable(x),
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
    let real_path = if path.is_dir() {
      path.join("default.nix")
    } else {
      path.to_path_buf()
    };
    let path = &real_path;

    let fec = self.file_eval_cache.lock();

    if let Some(v2) = fec.get(path) {
      return Ok(v2.read().clone());
    }

    drop(fec);

    let mut fpc = self.file_parse_cache.lock();

    let e = match fpc.get(path) {
      None => {
        let parsed = crate::syntax::parse_from_file(path, &self.static_base_env)?;
        let e = readable(parsed);
        trace!("done parsing {}", path.display());
        fpc.insert(path.to_path_buf(), e.clone());
        e
      }
      Some(x) => x.clone(),
    };

    drop(fpc);

    let v = self.eval(&self.base_env, &*e)?;

    self
      .file_eval_cache
      .lock()
      .insert(path.to_path_buf(), writable(v.clone()));

    Ok(v)
  }

  pub fn eval_inline<S: AsRef<str>>(&self, input: S) -> Result<Value> {
    let parsed = crate::syntax::parse_inline(input.as_ref(), &self.static_base_env)?;

    self.eval(&self.base_env, &parsed)
  }

  fn maybe_thunk(&self, e: ExprRef, env: EnvRef) -> Result<ValueRef> {
    match &*e {
      Expr::Var(v) => {
        let vread = v.read();
        if let Some(x) = self.lookup_var(env.clone(), &*vread, true)? {
          Ok(x)
        } else {
          drop(vread);
          Ok(writable(Value::thunk(env, e)))
        }
      }
      _ => Ok(writable(Value::thunk(env, e))),
    }
  }

  fn force_value<'v>(&self, m: &'v ValueRef, pos: Pos) -> Result<RwLockReadGuard<'v, Value>> {
    let readable = m.upgradable_read();
    if readable.needs_eval() {
      let mut upgraded = RwLockUpgradableReadGuard::upgrade(readable);
      let old_value = std::mem::replace(&mut *upgraded, Value::Blackhole);
      match old_value {
        Value::Thunk(Thunk { env, expr }) => {
          let result = self.eval(&env, &*expr)?;
          *upgraded = result;
        }
        Value::Apply(lhs, rhs) => {
          let thing = self.call_function(&lhs, &rhs, pos)?;
          *upgraded = thing;
        }
        _ => {}
      }
      Ok(RwLockWriteGuard::downgrade(upgraded))
    } else {
      Ok(RwLockUpgradableReadGuard::downgrade(readable))
    }
  }

  fn get_name(&self, attr: &AttrName, env: &EnvRef) -> Result<Ident> {
    match attr {
      AttrName::Static(s) => Ok(s.clone()),
      AttrName::Dynamic(e) => {
        let v = writable(self.eval(env, &*e)?);
        let name_str = self.force_string_no_context(&v, Pos::none())?;
        Ok(Ident::from(&*name_str))
      }
    }
  }

  fn call_function(&self, fun: &ValueRef, arg: &ValueRef, pos: Pos) -> Result<Value> {
    match &*self.force_value(fun, pos)? {
      Value::Attrs(ref aread) => {
        if let Some(functor) = aread.get(&ident!("__functor")) {
          let v2 = writable(self.call_function(&functor.v, fun, pos)?);
          self.call_function(&v2, arg, pos)
        } else {
          throw!(pos, "cannot call a value of type attrset as a function")
        }
      }
      Value::Primop(p, args) => {
        let mut args = args.clone();
        args.push(arg.clone());
        if p.arity as usize == args.len() {
          (p.fun)(self, pos, args)
        } else {
          Ok(Value::Primop(p.clone(), args))
        }
      }
      Value::Lambda(e, lam) => self.call_lambda(e.clone(), lam, pos, arg),
      v => throw!(
        pos,
        "cannot call a value of type {} as a function",
        v.typename()
      ),
    }
  }

  fn call_lambda(&self, env: EnvRef, lam: &Lambda, pos: Pos, arg: &ValueRef) -> Result<Value> {
    let mut env2 = Env::default();
    env2.up = Some(env);
    let env2 = writable(env2);

    let mut attrs_used = 0;

    if let LambdaArg::Formals { name, formals } = &lam.arg {
      let fs = self.force_attrs(&arg, pos)?;

      if name.is_some() {
        env2.write().values.push(arg.clone());
      }

      for formal in &formals.formals {
        if let Some(argval) = fs.get(&formal.name) {
          attrs_used += 1;
          env2.write().values.push(argval.v.clone());
        } else if let Some(ref x) = formal.def {
          let fallback = self.maybe_thunk(x.clone(), env2.clone())?;
          env2.write().values.push(fallback);
        } else {
          throw!(
            pos,
            "{} called without required argument `{}'",
            lam.name,
            formal.name
          )
        }
      }

      if !formals.ellipsis && attrs_used != fs.len() {
        todo!("called with unused argument")
      }
    } else {
      env2.write().values.push(arg.clone());
    }

    self.eval(&env2, &lam.body)
  }

  fn lookup_var(&self, mut env: EnvRef, var: &Var, no_eval: bool) -> Result<Option<ValueRef>> {
    trace!("resolving {}", var.name);
    match var.displ.level() {
      Some(l) => {
        if let Some(x) = NonZeroU8::new(l as _) {
          env = Env::climb(env, x);
        }
      }
      None => bail!("unresolved: {}", var.name),
    }

    if let Displacement::Static { offset, .. } = var.displ {
      return Ok(env.read().values.get(offset).cloned());
    }

    loop {
      let mut env_ = env.write();
      if env_.env_type == EnvType::HasWithExpr {
        if no_eval {
          return Ok(None);
        }
        let _ = self.force_attrs(&env_.values[0], Pos::none())?;
        env_.env_type = EnvType::HasWithAttrs;
      }
      if let Some(j) = env_.values[0].read().as_attrs() {
        if let Some(j) = j.get(&var.name) {
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
        None => throw!(var.pos, "undefined variable `{}'", var.name),
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
      Arc::get_mut(be.values[0].write().as_attrs_mut().unwrap())
        .unwrap()
        .insert(Ident::from(name2), not_located(v.clone()));
    }
    Ok(v)
  }

  fn add_primop(&self, name: &str, arity: u8, fun: PrimopFn) -> Result<ValueRef> {
    let sym = Ident::from(name.strip_prefix("__").unwrap_or(name));

    if arity == 0 {
      bail!("arity = 0")
    }

    let v = writable(Value::Primop(
      Primop {
        fun,
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
    Arc::get_mut(
      be.values[0]
        .write()
        .as_attrs_mut()
        .expect("builtins is gone 🦀"),
    )
    .unwrap()
    .insert(sym, not_located(v.clone()));

    Ok(v)
  }

  fn build_attrs(&self, env: EnvRef, attrs: &expr::Attrs) -> Result<Value> {
    let mut dynamic_env = env.clone();
    let mut new_attrs = Attrs::new();

    if attrs.recursive {
      let mut env2 = Env::default();
      env2.up = Some(env.clone());
      let env2 = writable(env2);
      dynamic_env = env2.clone();

      let has_overrides = attrs.attrs.contains_key(&ident!("__overrides"));

      for (displ, (k, v)) in attrs.attrs.iter().enumerate() {
        let v_attr = if has_overrides && !v.inherited {
          writable(Value::thunk(env2.clone(), v.rhs.clone()))
        } else {
          self.maybe_thunk(
            v.rhs.clone(),
            if v.inherited {
              env.clone()
            } else {
              env2.clone()
            },
          )?
        };
        env2.write().values.insert(displ, v_attr.clone());
        new_attrs.insert(
          k.clone(),
          Located {
            pos: v.pos,
            v: v_attr,
          },
        );
      }
    } else {
      for (k, v) in &attrs.attrs {
        let new_rhs = Located {
          v: self.maybe_thunk(v.rhs.clone(), env.clone())?,
          pos: v.pos,
        };
        new_attrs.insert(k.clone(), new_rhs);
      }
    }

    for val in &attrs.dyn_attrs {
      let name = writable(self.eval(&dynamic_env, &val.name)?);
      if self.force_value(&name, val.pos)?.as_null().is_some() {
        continue;
      }

      let namestr = self.force_string_no_context(&name, val.pos)?;
      if new_attrs
        .insert(
          Ident::from(&*namestr),
          Located {
            pos: val.pos,
            v: self.maybe_thunk(val.value.clone(), dynamic_env.clone())?,
          },
        )
        .is_some()
      {
        throw!(val.pos, "dynamic attribute `{}' already defined", namestr);
      }
    }

    Ok(Value::Attrs(readable(new_attrs)))
  }

  fn select(
    &self,
    env: &EnvRef,
    pos: Pos,
    lhs: &Expr,
    def: Option<&Expr>,
    path: &[Located<AttrName>],
  ) -> Result<ValueRef> {
    let mut vtmp = writable(self.eval(env, lhs)?);
    let mut pos2 = pos;

    for name in path {
      let name = self.get_name(&name.v, env)?;
      if let Some(x) = def {
        let tmp2 = self.force_value(&vtmp, pos2)?;
        if let Some(x) = tmp2.as_attrs().and_then(|x| x.get(&name)) {
          let n = x.v.clone();
          drop(tmp2);
          vtmp = n;
          continue;
        }
        return Ok(writable(self.eval(env, x)?));
      } else {
        let tmp2 = self.force_attrs(&vtmp, pos2)?;
        if let Some(x) = tmp2.get(&name) {
          pos2 = x.pos;
          let n = x.v.clone();
          drop(tmp2);
          vtmp = n;
        } else {
          throw!(pos2, "attribute `{}' missing", name)
        }
      }
    }

    let _ = self.force_value(&vtmp, pos2)?;
    Ok(vtmp)
  }

  fn eval_bool(&self, env: &EnvRef, e: &Expr) -> Result<bool> {
    let v = writable(self.eval(env, e)?);
    let x = match &*self.force_value(&v, Pos::none())? {
      Value::Bool(b) => Ok(*b),
      q => bail!("expected bool, got {}", q.typename()),
    };
    x
  }

  fn eval_attrs(&self, env: &EnvRef, e: &Expr) -> Result<Readable<Attrs>> {
    match self.eval(env, e)? {
      Value::Attrs(attrs) => Ok(attrs),
      q => bail!("expected attrset, got {}", q.typename()),
    }
  }

  fn eval_list(&self, env: &EnvRef, e: &Expr) -> Result<Readable<Vec<ValueRef>>> {
    match self.eval(env, e)? {
      Value::List(l) => Ok(l),
      q => bail!("expected list, got {}", q.typename()),
    }
  }

  fn eval_operator(
    &self,
    env: &EnvRef,
    bin: Bin,
    lhs: &Expr,
    rhs: &Expr,
    pos: Pos,
  ) -> Result<Value> {
    Ok(match bin {
      Bin::And => Value::Bool(self.eval_bool(env, lhs)? && self.eval_bool(env, rhs)?),
      Bin::Or => Value::Bool(self.eval_bool(env, lhs)? || self.eval_bool(env, rhs)?),
      Bin::Impl => Value::Bool(!self.eval_bool(env, lhs)? || self.eval_bool(env, rhs)?),
      Bin::Eq => {
        let v1 = self.eval(env, lhs)?;
        let v2 = self.eval(env, rhs)?;
        Value::Bool(self.values_equal(&writable(v1), &writable(v2), pos)?)
      }
      Bin::Neq => {
        let v1 = self.eval(env, lhs)?;
        let v2 = self.eval(env, rhs)?;
        Value::Bool(!self.values_equal(&writable(v1), &writable(v2), pos)?)
      }
      Bin::Update => {
        let mut attrs1 = self.eval_attrs(env, lhs)?;
        let mut attrs2 = self.eval_attrs(env, rhs)?;

        trace!(
          "combining {:?} and {:?} at {:?}",
          attrs1.keys(),
          attrs2.keys(),
          pos
        );

        if attrs1.is_empty() {
          Value::Attrs(attrs2)
        } else if attrs2.is_empty() {
          Value::Attrs(attrs1)
        } else {
          Arc::make_mut(&mut attrs1).append(Arc::make_mut(&mut attrs2));

          trace!("...into {:?}", attrs1.keys());

          Value::Attrs(attrs1)
        }
      }
      Bin::ConcatLists => {
        let mut l1 = self.eval_list(env, lhs)?;
        let mut l2 = self.eval_list(env, rhs)?;
        Arc::make_mut(&mut l1).append(Arc::make_mut(&mut l2));
        Value::List(l1)
      }
    })
  }

  fn values_equal(&self, lhs: &ValueRef, rhs: &ValueRef, pos: Pos) -> Result<bool> {
    let lhs = &*self.force_value(lhs, pos)?;
    let rhs = &*self.force_value(rhs, pos)?;

    if let Some(pair) = numbers(lhs, rhs) {
      return Ok(match pair {
        Either::Left((a, b)) => a == b,
        Either::Right((a, b)) => (a - b).abs() < std::f64::EPSILON,
      });
    }

    Ok(match (lhs, rhs) {
      (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
      (Value::String(Str { s: s1, .. }), Value::String(Str { s: s2, .. })) => s1 == s2,
      (Value::Path(p1), Value::Path(p2)) => p1 == p2,
      (Value::Null, Value::Null) => true,

      (Value::Attrs(a1), Value::Attrs(a2)) => {
        if Arc::ptr_eq(&a1, &a2) {
          return Ok(true);
        }

        if a1.len() != a2.len() {
          return Ok(false);
        }

        for (k, v) in a1.iter() {
          if let Some(v2) = a2.get(k) {
            if !self.values_equal(&v.v, &v2.v, pos)? {
              return Ok(false);
            }
          } else {
            return Ok(false);
          }
        }

        true
      }
      (Value::List(l1), Value::List(l2)) => {
        if l1.len() != l2.len() {
          false
        } else {
          for (i, v) in l1.iter().enumerate() {
            if !self.values_equal(&v, &l2[i], pos)? {
              return Ok(false);
            }
          }

          true
        }
      }
      (Value::Lambda { .. }, _) => false,
      (Value::Primop { .. }, _) => false,

      _ => false,
    })
  }

  fn force_string<'v>(&self, v: &'v ValueRef, pos: Pos) -> Result<MappedRwLockReadGuard<'v, Str>> {
    RwLockReadGuard::try_map(self.force_value(&v, pos)?, Value::as_string)
      .map_err(|e| err!(pos, "expected a string, got {}", e.typename()))
  }

  fn force_string_no_context<'v>(
    &self,
    v: &'v ValueRef,
    pos: Pos,
  ) -> Result<MappedRwLockReadGuard<'v, str>> {
    let guard = self.force_string(v, pos)?;
    MappedRwLockReadGuard::try_map(guard, |s| {
      if s.context.is_empty() {
        Some(s.s.as_str())
      } else {
        None
      }
    })
    .map_err(|s| err!(pos, "the string `{}' may not refer to a store path", s.s))
  }

  fn force_attrs<'v>(&self, v: &'v ValueRef, pos: Pos) -> Result<MappedRwLockReadGuard<'v, Attrs>> {
    RwLockReadGuard::try_map(self.force_value(&v, pos)?, |v| v.as_attrs().map(|x| &**x))
      .map_err(|e| err!(pos, "expected attrset, got {}", e.typename()))
  }

  fn force_list<'v>(
    &self,
    v: &'v ValueRef,
    pos: Pos,
  ) -> Result<MappedRwLockReadGuard<'v, [ValueRef]>> {
    RwLockReadGuard::try_map(self.force_value(v, pos)?, |v| {
      v.as_list().map(|x| x.as_slice())
    })
    .map_err(|e| err!(pos, "expected list, got {}", e.typename()))
  }

  fn force_int(&self, v: &ValueRef, pos: Pos) -> Result<i64> {
    let lock = self.force_value(v, pos)?;
    lock
      .as_int()
      .copied()
      .ok_or_else(|| anyhow!("expected int, got {}", lock.typename()))
  }
}

fn numbers(v1: &Value, v2: &Value) -> Option<Either<(i64, i64), (f64, f64)>> {
  match (v1, v2) {
    (Value::Int(i1), Value::Int(i2)) => Some(Either::Left((*i1, *i2))),
    (Value::Int(i1), Value::Float(f2)) => Some(Either::Right((*i1 as _, *f2))),
    (Value::Float(f1), Value::Int(i2)) => Some(Either::Right((*f1, *i2 as _))),
    (Value::Float(f1), Value::Float(f2)) => Some(Either::Right((*f1, *f2))),
    _ => None,
  }
}

#[test]
fn test_nixpkgs_eval() -> Result<()> {
  crate::logger::init()?;

  let e = Eval::default();
  e.create_base_env()?;

  eprintln!(
    "{:?}",
    e.eval_inline("import /home/jude/.code/nix/pkgs/default.nix {}")
      .or_exit()
      .debug()
  );
  Ok(())
}

#[test]
fn test_attrs_equality() {
  crate::logger::init().unwrap();

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
