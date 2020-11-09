use std::{collections::HashMap, sync::Arc};

use self::{
  context::{Context, Scope, StaticScope},
  string::CoerceOpts,
  value::{arc, StringCtx, Value, ValueRef},
};
use crate::{
  prelude::*,
  syntax::expr::{self, AttrList, AttrName, Binding, Expr, ExprRef, InheritFrom, StrPart},
};
use codespan::Files;
use im::OrdSet;
use itertools::{Either, Itertools};
use parking_lot::{MappedRwLockReadGuard, Mutex, RwLockReadGuard, RwLockUpgradableReadGuard};

pub mod context;
mod primop;
mod string;
pub mod value;

#[cfg(test)] mod tests;

pub struct Eval {
  expr: crate::arena::Arena<Expr>,
  files: Mutex<Files<String>>,
  parse_cache: Mutex<HashMap<PathBuf, ExprRef>>,
}

impl Default for Eval {
  fn default() -> Self {
    Self::new()
  }
}

impl Eval {
  pub fn new() -> Self {
    Self {
      expr: crate::arena::Arena::new(),
      files: Mutex::new(Files::new()),
      parse_cache: Default::default(),
    }
  }

  pub fn load_file<P: AsRef<Path>>(&self, path: P) -> Result<ValueRef> {
    let path = path.as_ref().canonicalize()?;
    let mut cache = self.parse_cache.lock();
    let e = if let Some(x) = cache.get(&path) {
      *x
    } else {
      let contents = fs::read_to_string(&path)?;
      let mut f = self.files.lock();
      let id = f.add(&path, contents);
      let e = crate::syntax::parse(id, &self.expr, f.source(id))?;
      cache.insert(path, e);
      e
    };
    Ok(self.defer(e, &Default::default()))
  }

  pub fn load_inline<S: Into<String>>(&self, src: S) -> Result<ValueRef> {
    let mut f = self.files.lock();
    let id = f.add("<inline>", src.into());
    let e = crate::syntax::parse(id, &self.expr, f.source(id))?;
    Ok(self.defer(e, &Default::default()))
  }

  fn defer(&self, e: ExprRef, c: &Context) -> ValueRef {
    arc(match &self.expr[e.node] {
      Expr::Int(i) => Value::Int(*i),
      Expr::Float(f) => Value::Float(*f),
      Expr::List(items) => Value::List(items.elems.iter().map(|e| self.defer(*e, c)).collect()),
      _ => Value::Thunk(e, c.clone()),
    })
  }

  pub fn value<'v>(&self, v: &'v ValueRef) -> Result<RwLockReadGuard<'v, Value>> {
    while self.force(v)? {}
    Ok(v.read())
  }

  /// Returns `bool` indicating whether evaluation needed to be performed.
  fn force(&self, this: &ValueRef) -> Result<bool> {
    let lock = this.upgradable_read();
    if lock.should_evaluate() {
      let mut lock = RwLockUpgradableReadGuard::try_upgrade(lock).map_err(|_| {
        anyhow!("internal evaluator error: multiple write locks held on the same value")
      })?;

      // another thread got ahold of this thunk after is_thunk() returned, but before
      // the lock was upgraded, so we can assume they are evaluating it
      if !lock.should_evaluate() {
        return Ok(true);
      }

      let thunk = std::mem::replace(&mut *lock, Value::Blackhole);
      let new_vref = match thunk {
        Value::Thunk(e, c) => self.eval(e, &c)?,
        Value::Apply(lhs, rhs) => self.apply_function(&lhs, &rhs)?,
        _ => unreachable!(),
      };
      *lock = Arc::try_unwrap(new_vref).map_or_else(|a| a.read().clone(), |i| i.into_inner());
      return Ok(true);
    } else if lock.is_blackhole() {
      bail!("infinite loop during evaluation");
    }
    Ok(false)
  }

  fn forced(&self, e: ExprRef, c: &Context) -> Result<ValueRef> {
    let v = self.defer(e, c);
    self.force(&v)?;
    Ok(v)
  }

  fn eval(&self, id: ExprRef, context: &Context) -> Result<ValueRef> {
    match &self.expr[id.node] {
      Expr::Str(expr::Str { body, .. }) => {
        let mut buf = String::new();
        let mut path_context = OrdSet::new();
        for item in body {
          match item {
            StrPart::Plain(s) => {
              buf.push_str(s);
            }
            StrPart::Quote { quote, .. } => {
              let contents = self.coerce_to_string(
                &self.defer(*quote, context),
                &mut path_context,
                CoerceOpts {
                  extended: false,
                  copy_to_store: true,
                },
              )?;
              buf.push_str(&contents);
            }
          }
        }
        Ok(arc(Value::String(StringCtx {
          s: buf,
          context: path_context,
        })))
      }
      Expr::Var(x) => {
        for layer in &context.scopes {
          match &**layer {
            Scope::Static(s) => {
              if let Some(r) = s.get(x) {
                return Ok(Arc::clone(r));
              }
            }
            Scope::Dynamic(s) => {
              let scope = self.value_attrs_of(&s)?;
              if let Some(r) = scope.get(x) {
                return Ok(Arc::clone(r));
              }
            }
          }
        }
        for item in &context.with {
          let scope = self.value_attrs_of(item)?;
          if let Some(r) = scope.get(x) {
            return Ok(Arc::clone(r));
          }
        }
        bail!("unbound variable: {:?}", x)
      }
      Expr::AttrSet(attrs) => self.build_attrs(attrs.rec.is_some(), &attrs.attrs, context),
      Expr::Select(sel) => {
        let mut lhs = self.defer(sel.lhs, context);
        let mut failed = None;
        for path_item in &*sel.path.0 {
          let attrname = self.attrname_nonnull(path_item, context)?;
          match self.sel(&lhs, &attrname)? {
            Some(it) => {
              lhs = it;
            }
            None => {
              failed = Some(attrname);
              break;
            }
          }
        }
        if let Some(f) = failed {
          if let Some(o) = sel.or {
            self.eval(o.fallback, context)
          } else {
            bail!("Missing attribute {}", &f)
          }
        } else {
          Ok(lhs)
        }
      }
      Expr::With(expr::With { env, expr, .. }) => {
        let with_scope = self.defer(*env, context);
        self.eval(*expr, &context.add_with(with_scope))
      }
      Expr::Apply(expr::Apply { lhs, rhs }) => Ok(arc(Value::Apply(
        self.defer(*lhs, context),
        self.defer(*rhs, context),
      ))),
      e => bail!("unhandled expression: {:#?}", e),
    }
  }

  fn apply_function(&self, lhs: &ValueRef, rhs: &ValueRef) -> Result<ValueRef> {
    match &*self.value(lhs)? {
      Value::Lambda { fun, captures } => {
        self.call_lambda(&*fun.argument, fun.body, Some(rhs), &captures)
      }
      v => bail!("not a function: {:?}", v.typename()),
    }
  }

  fn call_lambda(
    &self,
    arg: &expr::LambdaArg,
    body: ExprRef,
    rhs: Option<&ValueRef>,
    context: &Context,
  ) -> Result<ValueRef> {
    let mut fn_body_scope = StaticScope::new();
    match arg {
      expr::LambdaArg::Plain(a) => {
        fn_body_scope.insert(
          a.clone(),
          match rhs {
            Some(tid) => Arc::clone(tid),
            None => bail!("trying to autocall a lambda with a plain argument"),
          },
        );
      }
      expr::LambdaArg::Formals(fs) => {
        let fn_arg_thunk = match rhs {
          Some(id) => Arc::clone(id),
          None => arc(Value::Attrs(StaticScope::new())),
        };
        let fn_argument = self.value_attrs_of(&fn_arg_thunk)?;
        let fn_scope_id = arc(Value::Blackhole);

        for arg in &fs.args {
          let name = &*arg.arg_name;
          match fn_argument.get(name) {
            None => {
              if let Some(expr::FormalDef { default, .. }) = arg.fallback {
                let def_arg = self.defer(
                  default,
                  &context.prepend(Scope::Dynamic(Arc::clone(&fn_scope_id))),
                );
                fn_body_scope.insert(name.clone(), def_arg);
              } else {
                bail!("Oh no")
              }
            }
            Some(id) => {
              fn_body_scope.insert(name.clone(), Arc::clone(id));
            }
          }
        }

        if let Some(expr::FormalsAt { ref name, .. }) = fs.at {
          fn_body_scope.insert((**name).clone(), Arc::clone(&fn_arg_thunk));
        }

        *fn_scope_id.write() = Value::Attrs(fn_body_scope.clone());
      }
    }

    self.eval(body, &context.prepend(Scope::Static(fn_body_scope)))
  }

  fn build_attrs(
    &self,
    recursive: bool,
    bindings: &[Spanned<Binding>],
    context: &Context,
  ) -> Result<ValueRef> {
    let env = arc(Value::Blackhole);

    let mut binds = StaticScope::new();

    let (mut regular, inherited): (Vec<_>, Vec<_>) =
      bindings.iter().partition_map(|bind| match bind.node {
        Binding::Plain { ref path, rhs, .. } => Either::Left((path, rhs)),
        Binding::Inherit {
          ref from,
          ref attrs,
          ..
        } => Either::Right((from.as_ref(), attrs)),
      });

    let recursive_scope = if recursive {
      context.prepend(Scope::Dynamic(Arc::clone(&env)))
    } else {
      context.clone()
    };

    for (from, attrs) in inherited {
      self.push_inherit(
        &mut binds,
        from,
        attrs,
        // rec { inherit foo; } tries to lookup `foo' within itself and causes an infinite loop
        // if we don't have this part :)
        if from.is_none() {
          &context
        } else {
          &recursive_scope
        },
      )?
    }

    let start_dyn = itertools::partition(&mut regular, |item| {
      (item.0).0.iter().any(|x| match x.node {
        AttrName::Plain(_) => true,
        // splices that only contain a string literal are handled specially by the upstream nix
        // parser, but since that's a bit complicated in lalrpop, we special-case it here instead
        AttrName::Dynamic { quote, .. } => self.expr[quote.node].is_plain_string(),
        _ => false,
      })
    });

    let (plain_attrs, splice_attrs) = regular.split_at(start_dyn);

    // add plain attributes first
    for (path, rhs) in plain_attrs {
      self.push_binding(&mut binds, &path.0[..], *rhs, &recursive_scope)?
    }

    *env.write() = Value::Attrs(binds.clone());

    if !splice_attrs.is_empty() {
      for (path, rhs) in splice_attrs {
        self.push_binding(&mut binds, &path.0[..], *rhs, &recursive_scope)?
      }

      return Ok(arc(Value::Attrs(binds)));
    }

    Ok(env)
  }

  fn push_binding(
    &self,
    scope: &mut StaticScope,
    names: &[Spanned<AttrName>],
    rhs: ExprRef,
    context: &Context,
  ) -> Result<()> {
    let (key1, keyrest) = names.split_first().unwrap();
    let key1 = self.attrname(key1, context)?;
    if let Some(key1) = key1 {
      let child_item = if keyrest.is_empty() {
        self.defer(rhs, context)
      } else {
        let mut next_scope = match scope.get(&key1) {
          Some(i) => unreachable!("{:?}", i), // self.value_attrs_of(*i)?.clone(),
          None => StaticScope::new(),
        };
        self.push_binding(&mut next_scope, keyrest, rhs, context)?;
        arc(Value::Attrs(next_scope))
      };
      scope.insert(key1, child_item);
    }
    Ok(())
  }

  fn push_inherit(
    &self,
    scope: &mut StaticScope,
    from: Option<&InheritFrom>,
    attrs: &AttrList,
    context: &Context,
  ) -> Result<()> {
    let binding_scope = match from {
      Some(ih) => Context::single(Scope::Dynamic(self.defer(ih.from, context))),
      None => context.clone(),
    };
    for attr in &attrs.0 {
      let name = self.attrname_nonnull(attr, context)?;
      scope.insert(
        name.clone(),
        self.synthetic_variable(attr.span, name, &binding_scope),
      );
    }
    Ok(())
  }

  fn synthetic_variable(&self, span: FileSpan, name: Ident, context: &Context) -> ValueRef {
    arc(Value::Thunk(
      spanned(span, self.expr.alloc(Expr::Var(name))),
      context.clone(),
    ))
  }

  fn attrname(&self, a: &AttrName, context: &Context) -> Result<Option<Ident>> {
    match a {
      AttrName::Plain(p) => Ok(Some(p.clone())),
      AttrName::Str { body, .. } => {
        let mut buf = String::new();
        for item in body {
          match item {
            StrPart::Plain(s) => buf.push_str(s),
            StrPart::Quote { quote, .. } => {
              let t = self.forced(*quote, context)?;
              unreachable!("{:?}", t)
              // let value = self.value_string_of(t)?;
              // buf.push_str(&value);
            }
          }
        }
        Ok(Some(buf.into()))
      }
      AttrName::Dynamic { quote, .. } => match &*self.forced(*quote, context)?.read() {
        Value::Null => Ok(None),
        Value::String(StringCtx { s: string, context }) => {
          if context.is_empty() {
            Ok(Some(Ident::from(string.as_str())))
          } else {
            bail!("a store path cannot be coerced to an attribute name")
          }
        }
        x => bail!("expected string, got {:?}", x),
      },
    }
  }

  fn attrname_nonnull(&self, a: &AttrName, context: &Context) -> Result<Ident> {
    Ok(
      self
        .attrname(a, context)?
        .ok_or_else(|| anyhow::anyhow!("an attribute name cannot be null at this position"))?,
    )
  }

  fn sel(&self, lhs: &ValueRef, rhs: &Ident) -> Result<Option<ValueRef>> {
    Ok(match &*self.value(lhs)? {
      Value::Attrs(hs) => {
        let h = hs.get(rhs).map(|x| Arc::clone(x));
        h
      }
      _ => None,
    })
  }
}

macro_rules! type_accessor {
  ($(#[$m:meta])? $shortname:ident, $longname:literal, $output:ty, $($lhs:pat => $rhs:expr),+) => {
    impl Eval {
      paste::paste! {
        $(#[$m])?
        pub fn [< value_ $shortname _of >]<'v>(
          &self,
          v: &'v ValueRef,
        ) -> Result<MappedRwLockReadGuard<'v, $output>> {
          RwLockReadGuard::try_map(self.value(v)?, |x| match x {
            $($lhs => Some($rhs),)+
            _ => None
          })
          .map_err(|y| anyhow!("unexpected type; expected {}, got {}", $longname, y.typename()))
        }
      }
    }
  };
}

type_accessor!(attrs, "attrset", StaticScope, Value::Attrs(a) => a);
type_accessor!(
  string,
  "string without context",
  String,
  Value::String(StringCtx { s, context }) => if context.is_empty() {
    s
  } else {
    return None
  }
);
type_accessor!(with_context, "string with context", StringCtx, Value::String(s) => s);
