use self::{
  builtins::AssertionFailure,
  context::{Context, Scope, StaticScope},
  primop::{Primop, PrimopFn},
  string::CoerceOpts,
  value::{arc, Value, ValueRef},
};
use crate::{
  prelude::*,
  syntax::expr::{self, AttrList, AttrName, Binding, Expr, ExprRef, InheritFrom, StrPart},
};
use codespan::Files;
use codespan_reporting::{
  diagnostic::{Diagnostic, Label, LabelStyle},
  term::emit,
};
use itertools::{Either, Itertools};
use parking_lot::{MappedRwLockReadGuard, Mutex, RwLockReadGuard, RwLockUpgradableReadGuard};
use std::{
  collections::{BTreeSet, HashMap},
  sync::{atomic::AtomicU8, Arc},
};

mod builtins;
pub mod context;
mod operators;
mod primop;
mod string;
pub mod value;

#[cfg(test)] mod tests;

pub struct Eval {
  expr: crate::arena::Arena<Expr>,
  files: Mutex<Files<String>>,
  builtins: ValueRef,
  frames: Mutex<Vec<FileSpan>>,
  parse_cache: Mutex<HashMap<PathBuf, ExprRef>>,
  inline_counter: AtomicU8,
}

impl Default for Eval {
  fn default() -> Self {
    Self::new()
  }
}

impl Eval {
  pub fn new() -> Self {
    let eval = Self {
      expr: crate::arena::Arena::new(),
      files: Mutex::new(Files::new()),
      parse_cache: Default::default(),
      frames: Mutex::new(vec![]),
      builtins: arc(Value::Blackhole),
      inline_counter: AtomicU8::new(0),
    };
    let mut builtins = StaticScope::new();
    builtins.insert(Ident::from("builtins"), Arc::clone(&eval.builtins));
    builtins.insert(Ident::from("__nixPath"), arc(self::builtins::mk_nix_path()));

    builtins.insert("null".into(), arc(Value::Null));
    builtins.insert("true".into(), arc(Value::Bool(true)));
    builtins.insert("false".into(), arc(Value::Bool(false)));

    builtins.insert(
      Ident::from("abort"),
      Primop::one("abort", |eval, msg| {
        let (msg, _) = &*eval.value_with_context_of(msg)?;
        bail!("evaluation aborted with message: {}", msg)
      }),
    );
    builtins.insert(
      Ident::from("attrNames"),
      Primop::one("attrNames", Self::attrnames_prim),
    );
    builtins.insert(
      Ident::from("compareVersions"),
      crate::op2!("compareVersions", Self::compare_versions),
    );
    builtins.insert(
      Ident::from("foldl'"),
      crate::op3!("foldl'", Self::foldl_strict),
    );
    builtins.insert(
      Ident::from("genList"),
      crate::op2!("genList", Self::gen_list),
    );
    builtins.insert(Ident::from("getEnv"), Primop::one("getEnv", Self::get_env));
    builtins.insert(Ident::from("import"), Primop::one("import", Self::import));
    builtins.insert(
      Ident::from("intersectAttrs"),
      crate::op2!("intersectAttrs", Self::intersect_attrs),
    );
    builtins.insert(
      Ident::from("isAttrs"),
      Primop::one("isAttrs", |eval, val| {
        Ok(arc(Value::Bool(matches!(
          &*eval.value(val)?,
          Value::Attrs(_)
        ))))
      }),
    );
    builtins.insert(
      Ident::from("isFunction"),
      Primop::one("isFunction", |eval, val| {
        Ok(arc(Value::Bool(matches!(
          &*eval.value(val)?,
          Value::Lambda {..} | Value::Primop {..}
        ))))
      }),
    );
    builtins.insert(
      Ident::from("isString"),
      Primop::one("isString", |eval, val| {
        Ok(arc(Value::Bool(matches!(
          &*eval.value(val)?,
          Value::String(_)
        ))))
      }),
    );
    builtins.insert(
      Ident::from("length"),
      Primop::one("length", |e, i| {
        Ok(arc(Value::Int(e.value_list_of(i)?.len() as _)))
      }),
    );
    builtins.insert(
      Ident::from("listToAttrs"),
      Primop::one("listToAttrs", Self::list_to_attrs),
    );
    builtins.insert(
      Ident::from("nixVersion"),
      arc(Value::String(("2.3.7".into(), Default::default()))),
    );
    builtins.insert(Ident::from("map"), crate::op2!("map", Self::map_list));
    builtins.insert(
      Ident::from("removeAttrs"),
      crate::op2!("removeAttrs", Self::remove_attrs),
    );
    builtins.insert(
      Ident::from("replaceStrings"),
      crate::op3!("replaceStrings", Self::replace_strings),
    );
    builtins.insert(
      Ident::from("toString"),
      Primop::one("toString", |eval, val| {
        let mut ctx = BTreeSet::new();
        Ok(arc(Value::String((
          eval.coerce_to_string(
            val,
            &mut ctx,
            CoerceOpts {
              extended: true,
              copy_to_store: false,
            },
          )?,
          ctx,
        ))))
      }),
    );
    *eval.builtins.write() = Value::Attrs(builtins);
    eval
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
    let id = f.add(
      format!(
        "<inline-{}>",
        self
          .inline_counter
          .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
      ),
      src.into(),
    );
    let e = crate::syntax::parse(id, &self.expr, f.source(id))?;
    Ok(self.defer(e, &Default::default()))
  }

  fn defer(&self, e: ExprRef, c: &Context) -> ValueRef {
    arc(match &self.expr[e.node] {
      Expr::Int(i) => Value::Int(*i),
      Expr::Float(f) => Value::Float(*f),
      Expr::List(items) => Value::List(items.elems.iter().map(|e| self.defer(*e, c)).collect()),
      Expr::Lambda(l) => Value::Lambda {
        fun: l.clone(),
        captures: Box::new(c.clone()),
      },
      Expr::Path(expr::Path::Plain(p)) if Path::new(p).is_absolute() => {
        Value::Path(PathBuf::from(p))
      }
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
        Value::Thunk(e, c) => {
          self.frames.lock().push(e.span);
          let e = self.eval(e, &c)?;
          self.frames.lock().pop();
          e
        }
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
    while self.force(&v)? {}
    Ok(v)
  }

  fn eval(&self, id: ExprRef, context: &Context) -> Result<ValueRef> {
    match &self.expr[id.node] {
      Expr::Str(expr::Str { body, .. }) | Expr::IndStr(expr::IndStr { body, .. }) => {
        let mut buf = String::new();
        let mut path_context = BTreeSet::new();
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
        Ok(arc(Value::String((buf, path_context))))
      }
      Expr::Path(p) => match p {
        expr::Path::Plain(p) => {
          let pb = Path::new(p);
          if pb.is_absolute() {
            Ok(arc(Value::Path(pb.into())))
          } else {
            let files = self.files.lock();
            let filename = files.name(id.span.file_id);
            let dest = if filename.to_str().unwrap().starts_with("<inline-") {
              std::env::current_dir()?.join(pb)
            } else {
              PathBuf::from(filename).parent().unwrap().join(pb)
            };
            let thing = path_abs::PathAbs::new(dest)?;
            Ok(arc(Value::Path(thing.as_path().to_path_buf())))
          }
        }
        expr::Path::Home(_) => todo!(),
        expr::Path::Nix { path, .. } => {
          let nixpath = self.synthetic_variable(id.span, Ident::from("__nixPath"), &context);
          Ok(arc(Value::Path(
            self.find_file(&nixpath, &path[1..path.len() - 1])?,
          )))
        }
      },
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
        if let Some(y) = self.value_attrs_of(&self.builtins)?.get(x) {
          return Ok(Arc::clone(y));
        }
        for item in &context.with {
          let scope = self.value_attrs_of(item)?;
          if let Some(r) = scope.get(x) {
            return Ok(Arc::clone(r));
          }
        }
        bail!("unbound variable `{}'", x)
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
      Expr::Assert(expr::Assert { cond, expr, .. }) => {
        let cond_ = self.defer(*cond, context);
        if self.value_bool_of(&cond_)? {
          self.eval(*expr, context)
        } else {
          bail!(AssertionFailure(format!(
            "assertion failed at {:?}",
            cond.span
          )))
        }
      }
      Expr::Let(expr::Let { binds, rhs, .. }) => {
        let scope = self.build_attrs(true, &*binds, context)?;
        self.eval(*rhs, &context.prepend(Scope::Dynamic(scope)))
      }
      Expr::If(expr::If {
        cond, rhs1, rhs2, ..
      }) => {
        let cond = self.defer(*cond, context);
        if self.value_bool_of(&cond)? {
          self.eval(*rhs1, context)
        } else {
          self.eval(*rhs2, context)
        }
      }
      Expr::Binary(b) => self.binary(b, context),
      Expr::Unary(u) => self.unary(u, context),
      Expr::Member(expr::Member { lhs, path, .. }) => {
        let mut lhs = self.defer(*lhs, context);

        for path_item in &path.0 {
          let attr = self.attrname_nonnull(path_item, context)?;
          if let Some(item) = self.sel(&lhs, &attr)? {
            lhs = item;
          } else {
            return Ok(arc(Value::Bool(false)));
          }
        }

        Ok(arc(Value::Bool(true)))
      }
      Expr::Apply(expr::Apply { lhs, rhs }) => Ok(arc(Value::Apply(
        self.defer(*lhs, context),
        self.defer(*rhs, context),
      ))),
      Expr::Lambda(l) => Ok(arc(Value::Lambda {
        fun: l.clone(),
        captures: Box::new(context.clone()),
      })),
      e => bail!("unhandled expression: {:?}", e),
    }
  }

  fn apply_function(&self, lhs: &ValueRef, rhs: &ValueRef) -> Result<ValueRef> {
    match &*self.value(lhs)? {
      Value::Lambda { fun, captures } => {
        self.call_lambda(&*fun.argument, fun.body, Some(rhs), &captures)
      }
      Value::Primop(Primop {
        fun: PrimopFn::Static(f),
        ..
      }) => f(self, rhs),
      Value::Primop(Primop {
        fun: PrimopFn::Dynamic(f),
        ..
      }) => f(self, rhs),
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
        Value::String((string, context)) => {
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

  fn value_path_of(&self, value: &ValueRef) -> Result<PathBuf> {
    match &*self.value(value)? {
      Value::Path(p) => Ok(p.clone()),
      Value::String((string, _)) => Ok(PathBuf::from(string)),
      Value::Attrs(a) if a.contains_key(&Ident::from("outPath")) => {
        self.value_path_of(&a[&Ident::from("outPath")])
      }
      v => bail!("wrong type: expected path, got {}", v.typename()),
    }
  }

  pub fn print_error(&self, e: Error) -> Result<()> {
    let files = self.files.lock();
    let trace = self.frames.lock();
    let diagnostic = Diagnostic::error()
      .with_message(format!("{:?}", e))
      .with_labels(
        trace
          .iter()
          .rev()
          .enumerate()
          .map(|(i, span)| {
            Label::new(
              if i == 0 {
                LabelStyle::Primary
              } else {
                LabelStyle::Secondary
              },
              span.file_id,
              span.span,
            )
            .with_message("while evaluating this expression")
          })
          .collect(),
      );
    emit(
      &mut termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto),
      &Default::default(),
      &*files,
      &diagnostic,
    )?;
    Ok(())
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

macro_rules! type_accessor_copy {
  ($(#[$m:meta])? $shortname:ident, $longname:literal, $output:ty, $($lhs:pat => $rhs:expr),+) => {
    impl Eval {
      paste::paste! {
        $(#[$m])?
        pub fn [< value_ $shortname _of >]<'v>(
          &self,
          v: &'v ValueRef,
        ) -> Result<$output> {
          match &*self.value(v)? {
            $($lhs => Ok($rhs),)+
            v => bail!("unexpected type; expected {}, got {}", $longname, v.typename())
          }
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
  Value::String((s, context)) => if context.is_empty() {
    s
  } else {
    return None
  }
);
type_accessor!(with_context, "string with context", (String, BTreeSet<String>), Value::String(s) => s);
type_accessor!(list, "list", [ValueRef], Value::List(l) => l.as_slice());

type_accessor_copy!(bool, "boolean", bool, Value::Bool(b) => *b);
type_accessor_copy!(int, "integer", i64, Value::Int(i) => *i);
type_accessor_copy!(float, "float", f64, Value::Float(i) => *i);
