use crate::{
  arena::Arena,
  prelude::{Path, *},
  store::LocalStore,
  syntax::expr::{self, *},
};
use builtins::strings::coerce_to_string;
use codespan::Files;
use codespan_reporting::{
  diagnostic::{Diagnostic, Label, LabelStyle},
  term::emit,
};
use config::Config;
use context::{Context, Scope, StaticScope};
use itertools::{Either, Itertools};
use log::Level;
use primop::{Op, Primop};
use std::{
  cell::RefCell,
  collections::{HashMap, HashSet, VecDeque},
  fs,
  sync::{
    atomic::{AtomicU16, Ordering},
    Arc, Mutex,
  },
};
use termcolor::{ColorChoice, StandardStream};
use thunk::{Thunk, ThunkCell, ThunkId};
use value::{PathSet, Value};

pub mod builtins;
mod config;
pub mod context;
pub mod operators;
pub mod primop;
pub mod thunk;
pub mod value;

#[derive(thiserror::Error, Debug)]
#[error("assertion failure: {}", message)]
struct AssertFailure {
  message: String,
}

pub struct Eval {
  items: Arena<Thunk>,
  expr: Arena<Expr>,
  toplevel: StaticScope,
  inline_counter: AtomicU16,
  files: Mutex<Files<String>>,
  file_ids: Mutex<HashMap<PathBuf, ThunkId>>,
  trace: RefCell<VecDeque<FileSpan>>,
  writer: StandardStream,
  config: Config,
  pub store: Arc<dyn Store>,
}

unsafe impl Send for Eval {}
unsafe impl Sync for Eval {}

impl Eval {
  pub fn new() -> Result<Self> {
    Self::with_config(Config {
      trace: std::env::var("NIX_TRACE").map_or(false, |x| !x.is_empty()),
    })
  }

  pub fn with_config(config: Config) -> Result<Self> {
    let mut this = Self {
      items: Default::default(),
      expr: Default::default(),
      toplevel: Default::default(),
      inline_counter: Default::default(),
      files: Default::default(),
      file_ids: Default::default(),
      writer: StandardStream::stderr(ColorChoice::Auto),
      config,
      store: Arc::new(LocalStore::open()?),
      trace: Default::default(),
    };
    builtins::init_primops(&mut this)?;
    Ok(this)
  }

  pub fn print_error(&self, e: Error) -> Result<()> {
    let files = self.files.lock().unwrap();
    let trace = self.trace.borrow();
    let trace_limit = if self.config.trace { trace.len() } else { 1 };
    let diagnostic = Diagnostic::error()
      .with_message(format!("{:?}", e))
      .with_labels(
        trace
          .iter()
          .take(trace_limit)
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
      &mut self.writer.lock(),
      &Default::default(),
      &*files,
      &diagnostic,
    )?;
    Ok(())
  }

  pub fn load_file<P: AsRef<Path>>(&self, path: P) -> Result<ThunkId> {
    let path = path.as_ref().canonicalize()?;
    let mut ids = self.file_ids.lock().unwrap();
    if let Some(x) = ids.get(&path) {
      return Ok(*x);
    }
    let eid = {
      trace!("loading file: {}", path.display());
      let contents = fs::read_to_string(&path)?;
      let mut f = self.files.lock().unwrap();
      let id = f.add(&path, contents);
      crate::syntax::parse(id, &self.expr, f.source(id))?
    };
    let thunk_id = self
      .items
      .alloc(Thunk::new(ThunkCell::Expr(eid, Context::new())));
    ids.insert(path, thunk_id);
    Ok(thunk_id)
  }

  pub fn load_inline<S: Into<String>>(&self, src: S) -> Result<ThunkId> {
    let eid = {
      let mut f = self.files.lock().unwrap();
      let id = f.add(
        format!(
          "<inline-{}>",
          self.inline_counter.fetch_add(1, Ordering::Acquire)
        ),
        src.into(),
      );
      crate::syntax::parse(id, &self.expr, f.source(id))?
    };
    Ok(self.items.alloc(Thunk::thunk(eid, Context::new())))
  }

  pub async fn value_of(&self, mut thunk_id: ThunkId) -> Result<&Value> {
    let mut ids = HashSet::new();
    ids.insert(thunk_id);
    loop {
      let v = match self.items[thunk_id].value_ref() {
        Some(x) => x,
        None => {
          let thunk = &self.items[thunk_id];
          let expr = thunk.get_thunk();
          trace!(target: "eval::thunk", "{:?} => {:?}", thunk_id, &expr);
          let val = self.step_thunk(expr).await?;
          trace!(target: "eval::thunk", "output: {:?}", &val);
          thunk.put_value(val)
        }
      };
      match v {
        Value::Ref(r) => {
          thunk_id = *r;
          if !ids.insert(thunk_id) {
            bail!(
              "reference cycle: {:?}",
              ids
                .into_iter()
                .map(|x| (x, &self.items[x]))
                .collect::<Vec<_>>()
            );
          }
        }
        _ => break Ok(v),
      }
    }
  }

  async fn value_bool_of(&self, ix: ThunkId) -> Result<bool> {
    match self.value_of(ix).await? {
      Value::Bool(b) => Ok(*b),
      v => bail!("Wrong type: expected bool, got {}", v.typename()),
    }
  }

  async fn value_string_of(&self, ix: ThunkId) -> Result<&str> {
    match self.value_of(ix).await? {
      Value::String { string, context } => {
        if context.is_empty() {
          Ok(string)
        } else {
          bail!("string is not allowed to refer to a store path")
        }
      }
      v => bail!("wrong type: expected string without context, got {:?}", v),
    }
  }

  pub async fn value_with_context_of(&self, ix: ThunkId) -> Result<(&str, &PathSet)> {
    match self.value_of(ix).await? {
      Value::String { string, context } => Ok((string, context)),
      v => bail!(
        "wrong type: expected string with context, got {}",
        v.typename()
      ),
    }
  }

  #[async_recursion]
  async fn value_path_of(&self, ix: ThunkId) -> Result<&Path> {
    match self.value_of(ix).await? {
      Value::Path(p) => Ok(p.as_ref()),
      Value::String { string, .. } => Ok(Path::new(string)),
      Value::AttrSet(a) if a.contains_key(&Ident::from("outPath")) => {
        self.value_path_of(a[&Ident::from("outPath")]).await
      }
      v => bail!("wrong type: expected path, got {}", v.typename()),
    }
  }

  async fn value_attrs_of(&self, ix: ThunkId) -> Result<&StaticScope> {
    match self.value_of(ix).await? {
      Value::AttrSet(ref s) => Ok(s),
      v => bail!("Wrong type: expected attrset, got {:?}", v),
    }
  }

  async fn value_list_of(&self, ix: ThunkId) -> Result<&[ThunkId]> {
    match self.value_of(ix).await? {
      Value::List(ref v) => Ok(v),
      v => bail!("Wrong type: expected list, got {}", v.typename()),
    }
  }

  async fn value_int_of(&self, ix: ThunkId) -> Result<i64> {
    match self.value_of(ix).await? {
      Value::Int(i) => Ok(*i),
      v => bail!("Wrong type: expected list, got {}", v.typename()),
    }
  }

  pub fn new_value(&self, v: Value) -> ThunkId {
    self.items.alloc(Thunk::complete(v))
  }

  async fn step_thunk(&self, thunk: ThunkCell) -> Result<Value> {
    match thunk {
      ThunkCell::Expr(e, c) => self.step_eval(e, c).await,
      ThunkCell::Apply(loc, lhs, rhs) => {
        self.trace.borrow_mut().push_front(loc);
        let res = self.step_fn(lhs, rhs).await?;
        self.trace.borrow_mut().pop_front();
        Ok(res)
      }
      ThunkCell::Blackhole => bail!("infinite loop"),
    }
  }

  async fn step_eval(&self, e: ExprRef, context: Context) -> Result<Value> {
    self.trace.borrow_mut().push_front(e.span);
    let result = self.step_eval_impl(e, context).await?;
    self.trace.borrow_mut().pop_front();
    Ok(result)
  }

  #[async_recursion]
  async fn step_eval_impl(&self, e: ExprRef, context: Context) -> Result<Value> {
    if log_enabled!(target: "eval::entry", Level::Trace) {
      let fs = self.files.lock().unwrap();
      trace!(
        target: "eval::entry",
        "{}:{}\n  {}",
        fs.name(e.span.file_id).to_string_lossy(),
        fs.location(e.span.file_id, e.span.span.start())?
          .line
          .number(),
        preview(fs.source_slice(e.span.file_id, e.span.span)?)
      );
    }
    match &self.expr[e.node] {
      Expr::Int(n) => Ok(Value::Int(*n)),
      Expr::Str(Str { body, .. }) | Expr::IndStr(IndStr { body, .. }) => {
        let mut final_buf = String::new();
        let mut str_context = PathSet::new();
        for item in body {
          match item {
            StrPart::Plain(s) => final_buf.push_str(s),
            StrPart::Quote { quote, .. } => {
              let t = self.items.alloc(Thunk::thunk(*quote, context.clone()));
              let contents = coerce_to_string(self, t, &mut str_context, false, true).await?;
              final_buf.push_str(&contents);
            }
          }
        }
        Ok(Value::String {
          string: final_buf,
          context: str_context,
        })
      }
      Expr::Uri(u) => Ok(Value::string_bare(u.to_string())),
      Expr::Path(p) => match p {
        expr::Path::Plain(p) => {
          let pb = Path::new(p);
          if pb.is_absolute() {
            Ok(Value::Path(pb.into()))
          } else {
            let files = self.files.lock().unwrap();
            let filename = files.name(e.span.file_id);
            let dest = PathBuf::from(filename).parent().unwrap().join(pb);
            let thing = path_abs::PathAbs::new(dest)?;
            Ok(Value::Path(thing.as_path().to_path_buf()))
          }
        }
        expr::Path::Home(_) => todo!(),
        expr::Path::NixPath { path, .. } => {
          let nixpath = self.synthetic_variable(e.span, Ident::from("__nixPath"), &context);
          Ok(Value::Path(
            builtins::sys::find_file(self, nixpath, &path[1..path.len() - 1]).await?,
          ))
        }
      },
      Expr::Apply(Apply { lhs, rhs }) => {
        Ok(Value::Ref(self.items.alloc(Thunk::new(ThunkCell::Apply(
          lhs.span.merge(&rhs.span),
          self.items.alloc(Thunk::thunk(*lhs, context.clone())),
          self.items.alloc(Thunk::thunk(*rhs, context)),
        )))))
      }
      Expr::Lambda(l) => Ok(Value::Lambda {
        lambda: l.clone(),
        captures: Box::new(context),
      }),
      Expr::Var(ident) => {
        for item in &context.scopes {
          let scope = match item.as_ref() {
            Scope::Static(s1) => s1,
            Scope::Dynamic(s) => self.value_attrs_of(*s).await?,
          };
          if let Some(v) = scope.get(ident) {
            return Ok(Value::Ref(*v));
          }
        }
        if let Some(x) = self.toplevel.get(ident) {
          return Ok(Value::Ref(*x));
        }
        for item in &context.with {
          let scope = self.value_attrs_of(*item).await?;
          if let Some(v) = scope.get(ident) {
            return Ok(Value::Ref(*v));
          }
        }
        bail!("Unbound variable {}", ident)
      }
      Expr::AttrSet(AttrSet { rec, ref attrs, .. }) => {
        let new_attrs = self.build_attrs(rec.is_some(), attrs, &context).await?;
        Ok(Value::Ref(new_attrs))
      }
      Expr::List(List { elems, .. }) => {
        let ids = self.items.alloc_extend(
          elems
            .iter()
            .copied()
            .map(|elm| Thunk::thunk(elm, context.clone())),
        );
        Ok(Value::List(ids))
      }
      Expr::Select(Select { lhs, path, or, .. }) => {
        let mut lhs = self.items.alloc(Thunk::thunk(*lhs, context.clone()));
        let mut failed = None;
        for path_item in &*path.0 {
          let attrname = self.attrname_nonnull(path_item, &context).await?;
          match self.sel(lhs, &attrname).await? {
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
          if let Some(o) = or {
            self.step_eval(o.fallback, context).await
          } else {
            bail!("Missing attribute {}", &f)
          }
        } else {
          Ok(Value::Ref(lhs))
        }
      }
      Expr::Let(Let { binds, rhs, .. }) => {
        let attrset = self.build_attrs(true, &*binds, &context).await?;
        self
          .step_eval(*rhs, context.prepend(Scope::Dynamic(attrset)))
          .await
      }
      Expr::If(If {
        cond, rhs1, rhs2, ..
      }) => {
        let cond = self.items.alloc(Thunk::thunk(*cond, context.clone()));
        if self.value_bool_of(cond).await? {
          self.step_eval(*rhs1, context).await
        } else {
          self.step_eval(*rhs2, context).await
        }
      }
      Expr::With(With { env, expr, .. }) => {
        let with_scope = self.items.alloc(Thunk::thunk(*env, context.clone()));
        self.step_eval(*expr, context.add_with(with_scope)).await
      }
      Expr::Assert(Assert { cond, expr, .. }) => {
        let cond_ = self.items.alloc(Thunk::thunk(*cond, context.clone()));
        if self.value_bool_of(cond_).await? {
          self.step_eval(*expr, context).await
        } else {
          bail!(AssertFailure {
            message: format!("assertion failed at {:?}", cond.span)
          })
        }
      }
      Expr::Binary(b) => operators::eval_binary(self, b, context).await,
      Expr::Unary(u) => operators::eval_unary(self, u, context).await,
      Expr::Member(Member { lhs, path, .. }) => {
        let mut lhs = self.items.alloc(Thunk::thunk(*lhs, context.clone()));

        for path_item in &path.0 {
          let attr = self.attrname_nonnull(path_item, &context).await?;
          match self.sel(lhs, &attr).await? {
            Some(item) => {
              lhs = item;
            }
            None => return Ok(Value::Bool(false)),
          }
        }

        Ok(Value::Bool(true))
      }
      e => bail!("unhandled expression {:?}", e),
    }
  }

  async fn expect_fn(&self, item: ThunkId) -> Result<()> {
    match self.value_of(item).await? {
      Value::Lambda { .. } | Value::Primop(_) => Ok(()),
      Value::AttrSet(a) if a.contains_key(&Ident::from("__functor")) => Ok(()),
      v => bail!("expected a function, got {}", v.typename()),
    }
  }

  #[async_recursion]
  async fn step_fn(&self, lhs: ThunkId, rhs: ThunkId) -> Result<Value> {
    match self.value_of(lhs).await? {
      Value::Lambda { lambda, captures } => {
        trace!(target: "eval::fn", "lambda");
        self
          .call_lambda(&*lambda.argument, lambda.body, Some(rhs), captures)
          .await
      }
      Value::Primop(Primop {
        op: Op::Static(f), ..
      }) => {
        trace!(target: "eval::fn", "primop");
        f(self, rhs)
      }
      Value::Primop(Primop {
        op: Op::Dynamic(f), ..
      }) => {
        trace!(target: "eval::fn", "primop");
        f(self, rhs)
      }
      Value::Primop(Primop {
        op: Op::Async(f), ..
      }) => {
        trace!(target: "eval::fn", "primop");
        f(self, rhs).await
      }
      Value::AttrSet(a) => {
        if let Some(ftor) = a.get(&Ident::from("__functor")) {
          trace!(target: "eval::fn", "__functor {:?}", *ftor);
          let inter_1 = self.step_fn(*ftor, lhs).await?;
          let inter_id = self.new_value(inter_1);
          self.step_fn(inter_id, rhs).await
        } else {
          bail!("an attrset cannot be called unless it has a `__functor' attribute")
        }
      }
      x => bail!("not a lambda: {:?}", x),
    }
  }

  async fn call_lambda(
    &self,
    arg: &LambdaArg,
    body: ExprRef,
    rhs: Option<ThunkId>,
    context: &Context,
  ) -> Result<Value> {
    let mut fn_body_scope = StaticScope::new();

    if log_enabled!(target: "eval::lambda", Level::Trace) {
      let fs = self.files.lock().unwrap();
      trace!(
        target: "eval::lambda",
        "{}:{}\n  {}",
        fs.name(body.span.file_id).to_string_lossy(),
        fs.location(body.span.file_id, body.span.span.start())?
          .line
          .number(),
        preview(fs.source_slice(body.span.file_id, body.span.span)?)
      );
    }

    match arg {
      LambdaArg::Plain(a) => {
        fn_body_scope.insert(
          a.clone(),
          match rhs {
            Some(tid) => tid,
            None => bail!("trying to autocall a lambda with a plain argument"),
          },
        );
      }
      LambdaArg::Formals(fs) => {
        let fn_arg_thunk = match rhs {
          Some(id) => id,
          None => self.new_value(Value::AttrSet(StaticScope::new())),
        };
        let fn_argument = self.value_attrs_of(fn_arg_thunk).await?;
        let fn_scope_id = self.items.alloc(Thunk::new(ThunkCell::Blackhole));

        for arg in &fs.args {
          let name = &*arg.arg_name;
          match fn_argument.get(name) {
            None => {
              if let Some(FormalDef { default, .. }) = arg.fallback {
                let def_arg = self.items.alloc(Thunk::thunk(
                  default,
                  context.prepend(Scope::Dynamic(fn_scope_id)),
                ));
                fn_body_scope.insert(name.clone(), def_arg);
              } else {
                bail!("Oh no")
              }
            }
            Some(id) => {
              fn_body_scope.insert(name.clone(), *id);
            }
          }
        }

        if let Some(FormalsAt { ref name, .. }) = fs.at {
          fn_body_scope.insert((**name).clone(), fn_arg_thunk);
        }

        self.items[fn_scope_id].put_value(Value::AttrSet(fn_body_scope.clone()));
      }
    }

    self
      .step_eval(body, context.prepend(Scope::Static(fn_body_scope)))
      .await
  }

  async fn attrname_nonnull(&self, a: &AttrName, context: &Context) -> Result<Ident> {
    Ok(
      self
        .attrname(a, context)
        .await?
        .ok_or_else(|| anyhow::anyhow!("an attribute name cannot be null at this position"))?,
    )
  }

  async fn attrname(&self, a: &AttrName, context: &Context) -> Result<Option<Ident>> {
    match a {
      AttrName::Plain(p) => Ok(Some(p.clone())),
      AttrName::Str { body, .. } => {
        let mut buf = String::new();
        for item in body {
          match item {
            StrPart::Plain(s) => buf.push_str(s),
            StrPart::Quote { quote, .. } => {
              let t = self.items.alloc(Thunk::thunk(*quote, context.clone()));
              let value = self.value_string_of(t).await?;
              buf.push_str(&value);
            }
          }
        }
        Ok(Some(buf.into()))
      }
      AttrName::Dynamic { quote, .. } => {
        let val = self.items.alloc(Thunk::thunk(*quote, context.clone()));
        match self.value_of(val).await? {
          Value::Null => Ok(None),
          Value::String { string, context } => {
            if context.is_empty() {
              Ok(Some(Ident::from(string.as_str())))
            } else {
              bail!("a store path cannot be coerced to an attribute name")
            }
          }
          x => bail!("expected string, got {}", x.typename()),
        }
      }
    }
  }

  async fn sel(&self, lhs: ThunkId, rhs: &Ident) -> Result<Option<ThunkId>> {
    Ok(match self.value_of(lhs).await? {
      Value::AttrSet(hs) => {
        let h = hs.get(rhs).copied();
        h
      }
      _ => None,
    })
  }

  async fn build_attrs(
    &self,
    recursive: bool,
    bindings: &[Spanned<Binding>],
    context: &Context,
  ) -> Result<ThunkId> {
    let env = self.items.alloc(Thunk::new(ThunkCell::Blackhole));

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
      context.prepend(Scope::Dynamic(env))
    } else {
      context.clone()
    };

    for (from, attrs) in inherited {
      self
        .push_inherit(
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
        )
        .await?
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
      self
        .push_binding(&mut binds, &path.0[..], *rhs, &recursive_scope)
        .await?
    }

    self.items[env].put_value(Value::AttrSet(binds.clone()));

    if !splice_attrs.is_empty() {
      for (path, rhs) in splice_attrs {
        self
          .push_binding(&mut binds, &path.0[..], *rhs, &recursive_scope)
          .await?
      }

      return Ok(self.new_value(Value::AttrSet(binds)));
    }

    Ok(env)
  }

  #[async_recursion]
  async fn push_binding(
    &self,
    scope: &mut StaticScope,
    names: &[Spanned<AttrName>],
    rhs: ExprRef,
    context: &Context,
  ) -> Result<()> {
    let (key1, keyrest) = names.split_first().unwrap();
    let key1 = self.attrname(key1, context).await?;
    if let Some(key1) = key1 {
      let child_item = if keyrest.is_empty() {
        self.items.alloc(Thunk::thunk(rhs, context.clone()))
      } else {
        let mut next_scope = match scope.get(&key1) {
          Some(i) => self.value_attrs_of(*i).await?.clone(),
          None => StaticScope::new(),
        };
        self
          .push_binding(&mut next_scope, keyrest, rhs, context)
          .await?;
        self.new_value(Value::AttrSet(next_scope))
      };
      scope.insert(key1, child_item);
    }
    Ok(())
  }

  async fn push_inherit(
    &self,
    scope: &mut StaticScope,
    from: Option<&InheritFrom>,
    attrs: &AttrList,
    context: &Context,
  ) -> Result<()> {
    let binding_scope = match from {
      Some(ih) => Context::single(Scope::Dynamic(
        self.items.alloc(Thunk::thunk(ih.from, context.clone())),
      )),
      None => context.clone(),
    };
    for attr in &attrs.0 {
      let name = self.attrname_nonnull(attr, context).await?;
      scope.insert(
        name.clone(),
        self.synthetic_variable(attr.span, name, &binding_scope),
      );
    }
    Ok(())
  }

  fn synthetic_variable(&self, span: FileSpan, name: Ident, context: &Context) -> ThunkId {
    self.items.alloc(Thunk::thunk(
      spanned(span, self.expr.alloc(Expr::Var(name))),
      context.clone(),
    ))
  }
}

fn preview(s: &str) -> &str {
  if s.len() <= 500 {
    s
  } else {
    &s[..500]
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  macro_rules! assert_eval {
    ($l:literal, $p:pat) => {
      let eval = Eval::new().unwrap();
      assert_matches::assert_matches!(eval.value_of(eval.load_inline($l)?).await, $p);
    };
  }

  #[async_std::test]
  async fn test_rec_order() -> Result<()> {
    assert_eval!(
      r#"rec { ${"${b}a"} = 1; ${"b"} = "a"; }.aa"#,
      Ok(Value::Int(1))
    );
    assert_eval!(r#"rec { ${"a"} = 1; b = a; }.b"#, Ok(Value::Int(1)));
    assert_eval!(r#"rec { inherit (a) b; a.b = 3; }.b"#, Ok(Value::Int(3)));
    assert_eval!(r#"with { a = 1; }; with { a = 2; }; a"#, Ok(Value::Int(2)));
    Ok(())
  }

  #[async_std::test]
  async fn test_foldl() -> Result<()> {
    let e = Eval::new().unwrap();
    let expr = e.load_inline(r#"builtins.foldl' (x: y: "${x}-${y}") "foo" ["bar" "baz" "qux"]"#)?;
    assert_eq!(e.value_string_of(expr).await?, "foo-bar-baz-qux");
    Ok(())
  }
}
