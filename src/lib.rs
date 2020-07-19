#![feature(untagged_unions)]

#[macro_use] extern crate async_recursion;

use arena::Arena;
use async_std::{
  path::{Path, PathBuf},
  sync::Mutex,
};
use codespan::Files;
use std::{
  borrow::Cow,
  sync::atomic::{AtomicU16, Ordering},
};
use syntax::{
  expr::{self, *},
  span::{spanned, FileSpan, Spanned},
};

mod builtins;
mod config;
mod error;
mod ext;
mod operators;
mod primop;
mod thunk;
mod value;

use codespan_reporting::{
  diagnostic::{Diagnostic, Label, LabelStyle},
  term::emit,
};
use config::Config;
use error::*;
use ext::*;
use primop::Primop;
use termcolor::{ColorChoice, StandardStream};
use thunk::*;
use value::{PathSet, Value};

pub struct Eval {
  items: Arena<Thunk>,
  expr: Arena<Expr>,
  toplevel: StaticScope,
  inline_counter: AtomicU16,
  files: Mutex<Files<String>>,
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
    Self::with_config(Default::default())
  }

  pub fn with_config(config: Config) -> Self {
    let mut this = Self {
      items: Default::default(),
      expr: Default::default(),
      toplevel: Default::default(),
      inline_counter: Default::default(),
      files: Default::default(),
      writer: StandardStream::stderr(ColorChoice::Auto),
      config,
    };
    this.toplevel.insert(
      "import".into(),
      this.items.alloc(Thunk::complete(Value::Primop(Primop {
        name: "import".into(),
        op: Box::new(move |e, i| Box::pin(builtins::import(e, i))),
      }))),
    );
    this.toplevel.insert(
      "abort".into(),
      this.items.alloc(Thunk::complete(Value::Primop(Primop {
        name: "abort".into(),
        op: Box::new(move |e, i| Box::pin(builtins::nix_abort(e, i))),
      }))),
    );
    this.toplevel.insert(
      "toString".into(),
      this.items.alloc(Thunk::complete(Value::Primop(Primop {
        name: "toString".into(),
        op: Box::new(move |e, i| Box::pin(builtins::coerce_to_string(e, i))),
      }))),
    );
    let nixver = this
      .items
      .alloc(Thunk::complete(Value::string_bare("2.3.7")));
    this.toplevel.insert(
      "__nixPath".into(),
      this
        .items
        .alloc(Thunk::complete(builtins::mk_nix_path(&this))),
    );
    this.toplevel.insert(
      "builtins".into(),
      this.items.alloc(Thunk::complete(Value::AttrSet({
        let mut builtins = StaticScope::new();
        builtins.insert("nixVersion".into(), nixver);
        builtins.insert(
          "compareVersions".into(),
          this.items.alloc(Thunk::complete(Value::Primop(Primop {
            name: "compareVersions".into(),
            op: Box::new(move |e, i| Box::pin(builtins::compare_primop(e, i))),
          }))),
        );
        builtins.insert(
          "removeAttrs".into(),
          this.items.alloc(Thunk::complete(Value::Primop(Primop {
            name: "removeAttrs".into(),
            op: Box::new(move |e, i| Box::pin(builtins::remove_attrs_primop(e, i))),
          }))),
        );
        builtins
      }))),
    );
    this
  }

  pub async fn print_error(&self, e: Error) -> Result<()> {
    let files = self.files.lock().await;
    let diagnostic = Diagnostic::error()
      .with_message(e.err.to_string())
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
    let eid = {
      let path = path.as_ref();
      let contents = async_std::fs::read_to_string(path).await?;
      let mut f = self.files.lock().await;
      let id = f.add(path, contents);
      syntax::parse(id, &self.expr, f.source(id))?
    };
    Ok(
      self
        .items
        .alloc(Thunk::new(ThunkCell::Expr(eid, Context::new()))),
    )
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
  pub async fn value_of(&self, t: ThunkId) -> Result<&Value> {
    let v = match self.items[t].value_ref() {
      Some(x) => x,
      None => {
        let thunk = self.items[t].get_thunk();
        let val = self.step_thunk(thunk).await?;
        self.items[t].put_value(val)
      }
    };
    match v {
      Value::Ref(r) => self.value_of(*r).await,
      _ => Ok(v),
    }
  }

  async fn value_bool_of(&self, ix: ThunkId) -> Result<bool> {
    match self.value_of(ix).await? {
      Value::Bool(b) => Ok(*b),
      v => bail!("Wrong type: expected string, got {}", v.typename()),
    }
  }

  async fn value_str_of(&self, ix: ThunkId) -> Result<(Cow<'_, str>, Option<&PathSet>)> {
    match self.value_of(ix).await? {
      Value::String {
        ref string,
        ref context,
      } => Ok((Cow::Borrowed(string), Some(context))),
      Value::Path(p) => Ok((Cow::Owned(p.display().to_string()), None)),
      v => bail!("Wrong type: expected string, got {}", v.typename()),
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

  async fn step_thunk(&self, thunk: ThunkCell) -> Result<Value> {
    match thunk {
      ThunkCell::Expr(e, c) => self.step_eval(e, c).await,
      ThunkCell::Apply(lhs, rhs) => self.step_fn(lhs, rhs).await,
      ThunkCell::Blackhole => bail!("internal evaluator error"),
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
              let (contents, paths) = self.value_str_of(t).await?;
              if let Some(p) = paths {
                str_context.extend(p.iter().cloned());
              }
              final_buf.push_str(&contents);
            }
          }
        }
        Ok(Value::String {
          string: final_buf,
          context: str_context,
        })
      }
      Expr::Path(p) => match p {
        expr::Path::Plain(p) => {
          let pb = Path::new(p);
          if pb.is_absolute() {
            Ok(Value::Path(pb.into()))
          } else {
            let pb = pb.strip_prefix(Path::new("./")).unwrap_or(pb);
            let files = self.files.lock().await;
            let filename = files.name(e.span.file_id);
            Ok(Value::Path(
              PathBuf::from(filename).parent().unwrap().join(pb),
            ))
          }
        }
        expr::Path::Home(_) => todo!(),
        expr::Path::NixPath { path, .. } => {
          let nixpath = self.synthetic_variable(e.span, Ident::from("__nixPath"), &context);
          Ok(Value::Path(
            builtins::find_file(self, nixpath, &path[1..path.len() - 1]).await?,
          ))
        }
      },
      Expr::Apply(Apply { lhs, rhs }) => {
        Ok(Value::Ref(self.items.alloc(Thunk::new(ThunkCell::Apply(
          self.items.alloc(Thunk::thunk(*lhs, context.clone())),
          self.items.alloc(Thunk::thunk(*rhs, context.clone())),
        )))))
      }
      Expr::Lambda(l) => Ok(Value::Lambda {
        lambda: l.clone(),
        captures: context.clone(),
      }),
      Expr::Var(ident) => {
        for item in &context {
          let scope = match item {
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
        self
          .step_eval(*expr, context.prepend(Scope::Dynamic(with_scope)))
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
      Value::Primop(Primop { op, .. }) => op(self, rhs).await,
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
          None => self
            .items
            .alloc(Thunk::complete(Value::AttrSet(StaticScope::new()))),
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
              let (value, _) = self.value_str_of(t).await?;
              buf.push_str(&value);
            }
          }
        }
        Ok(buf.into())
      }
      AttrName::Dynamic { quote, .. } => {
        let val = self.items.alloc(Thunk::thunk(*quote, context.clone()));
        let (s, _) = self.value_str_of(val).await?;
        Ok(s.into())
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
      self
        .items
        .alloc(Thunk::complete(Value::AttrSet(next_scope)))
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
      Some(ih) => Context::from(vec![Scope::Dynamic(
        self.items.alloc(Thunk::thunk(ih.from, context.clone())),
      )]),
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

  #[tokio::test]
  async fn test_foo() {
    let eval = Eval::new();
    let expr = eval
      .load_inline(r#"(import <nixpkgs> {}).hello"#)
      .await
      .expect("no parse");
    match eval.value_of(expr).await {
      Ok(x) => eprintln!("{:?}", x),
      Err(e) => eval.print_error(e).await.unwrap(),
    }
  }
}
