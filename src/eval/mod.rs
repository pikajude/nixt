use crate::{
  bail,
  error::{Error, Result, Traced},
  ext::ImmutVec,
  primop::{Op, Primop},
  thunk::{Context, Scope, StaticScope, Thunk, ThunkCell, ThunkId},
  value::{PathSet, Value},
  Config,
};
use arena::Arena;
use async_std::{
  fs,
  path::{Path, PathBuf},
  sync::Mutex,
};
use builtins::strings::coerce_to_string;
use codespan::Files;
use codespan_reporting::{
  diagnostic::{Diagnostic, Label, LabelStyle},
  term::emit,
};
use expr::{
  Apply, Assert, AttrList, AttrName, AttrSet, Binding, FormalDef, FormalsAt, If, InheritFrom,
  LambdaArg, Let, List, Member, Select, With,
};
use std::{
  collections::{HashMap, HashSet},
  sync::{
    atomic::{AtomicU16, Ordering},
    Arc,
  },
};
use syntax::{
  expr::{self, Expr, ExprRef, Ident, IndStr, Str, StrPart},
  span::{spanned, FileSpan, Spanned},
};
use termcolor::{ColorChoice, StandardStream};

mod builtins;
mod operators;

pub struct Eval {
  items: Arena<Thunk>,
  expr: Arena<Expr>,
  toplevel: StaticScope,
  inline_counter: AtomicU16,
  files: Mutex<Files<String>>,
  file_ids: Mutex<HashMap<PathBuf, ThunkId>>,
  writer: StandardStream,
  config: Config,
}

impl Default for Eval {
  fn default() -> Self {
    Self::new()
  }
}

impl Eval {
  pub fn new() -> Self {
    Self::with_config(Config {
      trace: std::env::var("NIX_TRACE").map_or(false, |x| !x.is_empty()),
    })
  }

  pub fn with_config(config: Config) -> Self {
    let mut this = Self {
      items: Default::default(),
      expr: Default::default(),
      toplevel: Default::default(),
      inline_counter: Default::default(),
      files: Default::default(),
      file_ids: Default::default(),
      writer: StandardStream::stderr(ColorChoice::Auto),
      config,
    };
    builtins::init_primops(&mut this);
    this
  }

  pub async fn print_error(&self, e: Error) -> Result<()> {
    let files = self.files.lock().await;
    let diagnostic = Diagnostic::error()
      .with_message(format!("{:?}", e.err))
      .with_labels(
        e.trace
          .into_iter()
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

  pub async fn load_file<P: AsRef<Path>>(&self, path: P) -> Result<ThunkId> {
    let path = path.as_ref().canonicalize().await?;
    let mut ids = self.file_ids.lock().await;
    if let Some(x) = ids.get(&path) {
      return Ok(*x);
    }
    let eid = {
      debug!("loading file: {}", path.display());
      let contents = fs::read_to_string(&path).await?;
      let mut f = self.files.lock().await;
      let id = f.add(&path, contents);
      syntax::parse(id, &self.expr, f.source(id))?
    };
    let thunk_id = self
      .items
      .alloc(Thunk::new(ThunkCell::Expr(eid, Context::new())));
    ids.insert(path, thunk_id);
    Ok(thunk_id)
  }

  pub async fn load_inline<S: Into<String>>(&self, src: S) -> Result<ThunkId> {
    let eid = {
      let mut f = self.files.lock().await;
      let id = f.add(
        format!(
          "<inline-{}>",
          self.inline_counter.fetch_add(1, Ordering::Acquire)
        ),
        src.into(),
      );
      syntax::parse(id, &self.expr, f.source(id))?
    };
    Ok(self.items.alloc(Thunk::thunk(eid, Context::new())))
  }

  #[async_recursion]
  pub async fn value_of(&self, mut thunk_id: ThunkId) -> Result<&Value> {
    let mut ids = HashSet::new();
    ids.insert(thunk_id);
    loop {
      let v = match self.items[thunk_id].value_ref() {
        Some(x) => x,
        None => {
          let thunk = &self.items[thunk_id];
          let val = self.step_thunk(thunk.get_thunk()).await?;
          thunk.put_value(val)
        }
      };
      match v {
        Value::Ref(r) => {
          thunk_id = *r;
          if !ids.insert(thunk_id) {
            bail!("reference cycle");
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

  async fn value_with_context_of(&self, ix: ThunkId) -> Result<(&str, &PathSet)> {
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
      v => bail!("Wrong type: expected attrset, got {}", v.typename()),
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

  // async fn value_float_cast(&self, ix: ThunkId) -> Result<f64> {
  //   match self.value_of(ix).await? {
  //     Value::Float(f1) => Ok(*f1),
  //     Value::Int(i1) => Ok(*i1 as f64),
  //     v => bail!("expected float, got {}", v.typename()),
  //   }
  // }

  async fn step_thunk(&self, thunk: ThunkCell) -> Result<Value> {
    match thunk {
      ThunkCell::Expr(e, c) => self.step_eval(e, c).await,
      ThunkCell::Apply(lhs, rhs) => self.step_fn(lhs, rhs).await,
      ThunkCell::Blackhole => bail!("infinite loop"),
    }
  }

  async fn step_eval(&self, e: ExprRef, context: Context) -> Result<Value> {
    self
      .step_eval_impl(e, context)
      .await
      .with_frame(e.span, self.config.trace)
  }

  #[async_recursion]
  async fn step_eval_impl(&self, e: ExprRef, context: Context) -> Result<Value> {
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
            let files = self.files.lock().await;
            let filename = files.name(e.span.file_id);
            let dest = PathBuf::from(filename).parent().unwrap().join(pb);
            let thing = path_abs::PathAbs::new(dest)?;
            Ok(Value::Path(thing.as_path().to_path_buf().into()))
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
          self.items.alloc(Thunk::thunk(*lhs, context.clone())),
          self.items.alloc(Thunk::thunk(*rhs, context)),
        )))))
      }
      Expr::Lambda(l) => Ok(Value::Lambda {
        lambda: l.clone(),
        captures: context,
      }),
      Expr::Var(ident) => {
        for item in &context {
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
        bail!("Unbound variable {}", ident)
      }
      Expr::AttrSet(AttrSet { rec, ref attrs, .. }) => {
        let new_attrs = self.items.alloc(Thunk::new(ThunkCell::Blackhole));
        self
          .build_attrs(rec.is_some(), attrs, new_attrs, &context)
          .await?;
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
          let attrname = self.attrname(path_item, &context).await?;
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
        let bindings = self.items.alloc(Thunk::new(ThunkCell::Blackhole));
        self.build_attrs(true, &*binds, bindings, &context).await?;
        self
          .step_eval(*rhs, context.prepend(Scope::Dynamic(bindings)))
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
        // XXX: `with` scope is checked *after* every other scope, not before
        self
          .step_eval(*expr, context.append(Scope::Dynamic(with_scope)))
          .await
      }
      Expr::Assert(Assert { cond, expr, .. }) => {
        let cond = self.items.alloc(Thunk::thunk(*cond, context.clone()));
        if self.value_bool_of(cond).await? {
          self.step_eval(*expr, context).await
        } else {
          bail!("assertion failed")
        }
      }
      Expr::Binary(b) => operators::eval_binary(self, b, context).await,
      Expr::Unary(u) => operators::eval_unary(self, u, context).await,
      Expr::Member(Member { lhs, path, .. }) => {
        let mut lhs = self.items.alloc(Thunk::thunk(*lhs, context.clone()));

        for path_item in &path.0 {
          let attr = self.attrname(path_item, &context).await?;
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

  async fn step_fn(&self, lhs: ThunkId, rhs: ThunkId) -> Result<Value> {
    match self.value_of(lhs).await? {
      Value::Lambda { lambda, captures } => {
        self
          .call_lambda(&*lambda.argument, lambda.body, Some(rhs), captures)
          .await
      }
      Value::Primop(Primop {
        op: Op::Static(f), ..
      }) => f(self, rhs),
      Value::Primop(Primop {
        op: Op::Async(f), ..
      }) => f(self, rhs).await,
      _ => todo!("not a lambda"),
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

  async fn attrname(&self, a: &AttrName, context: &Context) -> Result<Ident> {
    match a {
      AttrName::Plain(p) => Ok(p.clone()),
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
        Ok(buf.into())
      }
      AttrName::Dynamic { quote, .. } => {
        let val = self.items.alloc(Thunk::thunk(*quote, context.clone()));
        Ok(self.value_string_of(val).await?.into())
      }
    }
  }

  async fn sel(&self, lhs: ThunkId, rhs: &Ident) -> Result<Option<ThunkId>> {
    Ok(match self.value_of(lhs).await? {
      Value::AttrSet(hs) => hs.get(rhs).copied(),
      _ => None,
    })
  }

  async fn build_attrs(
    &self,
    recursive: bool,
    bindings: &[Spanned<Binding>],
    into: ThunkId,
    context: &Context,
  ) -> Result<()> {
    let mut binds = StaticScope::with_capacity(bindings.len());
    let recursive_scope = if recursive {
      context.prepend(Scope::Dynamic(into))
    } else {
      context.clone()
    };

    for b in bindings {
      match b.node {
        Binding::Plain { ref path, rhs, .. } => {
          self
            .push_binding(&mut binds, &path.0[..], rhs, &recursive_scope)
            .await?
        }
        Binding::Inherit {
          ref from,
          ref attrs,
          ..
        } => {
          self
            .push_inherit(&mut binds, from.as_ref(), attrs, context)
            .await?
        }
      }
    }

    self.items[into].put_value(Value::AttrSet(binds));

    Ok(())
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
      Some(ih) => Context::from(vec![Arc::new(Scope::Dynamic(
        self.items.alloc(Thunk::thunk(ih.from, context.clone())),
      ))]),
      None => context.clone(),
    };
    for attr in &attrs.0 {
      let name = self.attrname(attr, context).await?;
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

#[cfg(test)]
mod tests {
  use super::*;
  use std::ffi::OsString;

  async fn yoink_nixpkgs(into: &std::path::Path) -> Result<()> {
    eprintln!(
      "You don't have a <nixpkgs> available, so I will download the latest unstable channel."
    );
    let latest = std::io::Cursor::new(
      reqwest::Client::new()
        .get("https://channels.nixos.org/nixpkgs-unstable/nixexprs.tar.xz")
        .send()
        .await?
        .bytes()
        .await?,
    );
    let mut decoder = tar::Archive::new(xz2::read::XzDecoder::new(latest));
    for entry in decoder.entries()? {
      let mut entry = entry?;
      let dest: std::path::PathBuf = entry.path()?.components().skip(1).collect();
      entry.unpack(into.join(dest))?;
    }
    Ok(())
  }

  #[async_std::test]
  async fn test_foo() -> Result<()> {
    pretty_env_logger::init();

    if std::env::var("NIX_PATH").unwrap_or_default().is_empty() {
      let destdir = tempfile::tempdir()?.into_path();
      yoink_nixpkgs(&destdir).await?;
      let mut nix_path = OsString::from("nixpkgs=");
      nix_path.push(&destdir);
      eprintln!(
        "unpacked nixpkgs-unstable into {}",
        nix_path.to_string_lossy()
      );
      std::env::set_var("NIX_PATH", nix_path);
    }

    let eval = Eval::new();
    let expr = eval
      .load_inline(r#"(import <nixpkgs> { overlays = []; }).hello"#)
      .await?;
    match eval.value_of(expr).await {
      Ok(x) => eprintln!("{:p}", x),
      Err(e) => {
        eval.print_error(e).await?;
        panic!("eval failed")
      }
    }

    Ok(())
  }
}
