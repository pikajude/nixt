#[macro_use] extern crate log;

mod builtins;
pub mod error;
mod scope;
pub mod source_tree;
pub mod value;

use arena::Arena;
use async_recursion::async_recursion;
use codespan_reporting::{
  diagnostic::{Diagnostic, Label, LabelStyle},
  term::{
    termcolor::{ColorChoice, StandardStream},
    Config, DisplayStyle,
  },
};
use error::{Error, ErrorKind};
use expr::*;
use scope::{Context, Scope, StaticScope};
use source_tree::Source;
use std::{
  borrow::Cow,
  collections::HashMap,
  path::Path,
  sync::atomic::{AtomicUsize, Ordering},
  thread,
};
use syntax::{
  expr::{self, Expr, ExprRef},
  span::{FileSpan, Spanned},
};
use value::{Primop, Typename, Value, ValueRef};

pub type Result<T> = anyhow::Result<T, error::Error>;

pub struct Eval {
  allocator: Arena<Spanned<Value>>,
  source: Source,
  inherent_scope: ValueRef,
  inline_counter: AtomicUsize,
  stderr: StandardStream,
}

static_assertions::assert_impl_all!(Eval: Send);

macro_rules! mk_cast {
  ($f:ident, $output:ty, $expected:ident, $($p:pat => $v:expr),+) => {
    async fn $f(&self, v: ValueRef) -> Result<Spanned<$output>> {
      let Spanned { span, node } = self.forced(v).await?;
      Ok(Spanned::new(
        span,
        match node {
          $($p => $v,)+
          v => {
            return Err(
              ErrorKind::TypeMismatch {
                expected: Typename::$expected,
                actual: v.typename(),
              }
              .into(),
            )
          }
        },
      ))
    }
  };
}

impl Eval {
  mk_cast!(force_attrs, &StaticScope, Attrset, Value::Attrset(v) => v);

  mk_cast!(force_list, &[ValueRef], List, Value::List(l) => l);

  mk_cast!(force_str, &str, String, Value::String { s, .. } => s);

  mk_cast!(force_path_like, &Path, Path, Value::String { s, .. } => Path::new(s), Value::Path(p) => Path::new(p));

  pub fn new() -> Result<Self> {
    let source = Source::new();
    let builtins_id = source
      .files
      .lock()
      .unwrap()
      .add("builtins.nix", "<...>".into());
    let builtins_span = FileSpan::new(0u32, 0u32, builtins_id);
    let mut this = Self {
      allocator: Arena::new(),
      source,
      inline_counter: AtomicUsize::new(0),
      stderr: StandardStream::stderr(ColorChoice::Auto),
      inherent_scope: unsafe { std::mem::zeroed() }, // never read
    };
    let id = builtins::load_inherent_scope(&mut this, builtins_span)?;
    this.inherent_scope = id;
    Ok(this)
  }

  pub async fn load<P: AsRef<Path>>(&self, file: P) -> Result<ValueRef> {
    let ref0 = self.source.load_file(file).await?;
    Ok(self.allocator.insert(Spanned::new(
      ref0.span,
      Value::Thunk {
        id: ref0.node,
        context: Context::new(),
      },
    )))
  }

  pub async fn load_inline<S: AsRef<str>>(&self, code: S) -> Result<ValueRef> {
    let fakename = format!(
      "<inline-{}>",
      self.inline_counter.fetch_add(1, Ordering::Acquire)
    );
    let ref0 = self.source.load_inline(fakename, code.as_ref())?;
    Ok(self.allocator.insert(Spanned::new(
      ref0.span,
      Value::Thunk {
        id: ref0.node,
        context: Context::new(),
      },
    )))
  }

  fn thunk(&self, t: ExprRef, context: &Context) -> ValueRef {
    self.allocator.insert(Spanned::new(
      t.span,
      Value::Thunk {
        id: t.node,
        context: context.clone(),
      },
    ))
  }

  pub async fn force(&self, mut id: ValueRef) -> Result<()> {
    loop {
      let span = self.allocator.fetch(id, |x| x.span);
      let known_value = self.allocator.swap_deref(id, Value::blackhole());

      match known_value {
        Value::Blackhole(id) => {
          if id == thread::current().id() {
            return Err(ErrorKind::Loop.into());
          } else {
            break Ok(());
          }
        }
        Value::Pointer(p) => {
          self.allocator.swap_deref(id, known_value);
          id = p;
        }
        Value::App { lhs, rhs } => {
          let fn_result = self
            .call_function(lhs, rhs)
            .await
            .map_err(|e| e.add_frame(span))?;
          self.allocator.swap_deref(id, fn_result);
        }
        Value::Thunk {
          context,
          id: expr_id,
        } => {
          let new_value = self.step(expr_id, span, &context).await?;
          self.allocator.swap_deref(id, new_value);
        }
        _ => {
          self.allocator.swap_deref(id, known_value);
          break Ok(());
        }
      }
    }
  }

  async fn step(&self, id: ExprId, at: FileSpan, context: &Context) -> Result<Value> {
    self
      ._do_step(id, at, context)
      .await
      .map_err(|e| e.add_frame(at))
  }

  async fn _do_step(&self, id: ExprId, span: FileSpan, context: &Context) -> Result<Value> {
    match *self.source.expr(id) {
      Expr::Pos => {}
      Expr::Int(i) => return Ok(Value::Int(i)),
      Expr::Float(f) => return Ok(Value::Float(f)),
      Expr::Var(ref ident) => {
        return self.lookup_var(ident, context).await;
      }
      Expr::Str(_) => {}
      Expr::IndStr(_) => {}
      Expr::Path(ref p) => match p {
        expr::Path::Plain(_) => {}
        expr::Path::Home(_) => {}
        expr::Path::NixPath { path, .. } => {
          let strlit = self.allocator.insert(Spanned::new(
            span,
            Value::String {
              s: path[1..path.len() - 1].to_string(),
              context: Default::default(),
            },
          ));
          let nixpath = self
            .force_attrs(self.inherent_scope)
            .await?
            .get(&Ident::from("__nixPath"))
            .copied()
            .ok_or_else(|| ErrorKind::Unimplemented("attribute nixPath does not exist".into()))?;
          return Ok(Value::Path(
            builtins::find_file(self, nixpath, strlit).await?,
          ));
        }
      },
      Expr::Uri(_) => {}
      Expr::Lambda(ref l) => {
        return Ok(Value::Lambda {
          arg: l.argument.clone(),
          body: l.body,
          captures: context.clone(),
        })
      }
      Expr::Assert(_) => {}
      Expr::With(_) => {}
      Expr::Let(_) => {}
      Expr::List(_) => {}
      Expr::If(_) => {}
      Expr::Unary(_) => {}
      Expr::Binary(_) => {}
      Expr::Member(_) => {}
      Expr::Apply(expr::Apply { lhs, rhs }) => {
        return Ok(Value::App {
          lhs: self.thunk(lhs, context),
          rhs: self.thunk(rhs, context),
        })
      }
      Expr::Select(_) => {}
      Expr::AttrSet(AttrSet { rec, ref attrs, .. }) => {
        let new_attrset = self
          .allocator
          .insert(Spanned::new(span, Value::blackhole()));
        self
          .build_attrs(rec.is_some(), attrs.as_slice(), new_attrset, context)
          .await?;
        return Ok(Value::Pointer(new_attrset));
      }
    }

    todo!("{:?}", self.source.expr(id))
  }

  #[async_recursion]
  pub async fn forced(&self, id: ValueRef) -> Result<Spanned<&Value>> {
    self.force(id).await?;

    self.assume_init(id)
  }

  fn assume_init(&self, mut id: ValueRef) -> Result<Spanned<&Value>> {
    loop {
      let val = unsafe { self.allocator.index(id) };
      match val.node {
        Value::Thunk { .. } | Value::Blackhole { .. } | Value::App { .. } => {
          panic!("force() completed, but the value is not resolved")
        }
        Value::Pointer(p) => {
          id = p;
        }
        _ => break Ok(val.borrowed()),
      }
    }
  }

  async fn call_function(&self, lhs: ValueRef, rhs: ValueRef) -> Result<Value> {
    match self.forced(lhs).await?.node {
      Value::Lambda {
        arg,
        body,
        captures,
      } => self.call_lambda(arg, *body, Some(rhs), captures).await,
      Value::Primop(p) => match p {
        Primop::Static { op, .. } => Ok(op(self, rhs).await?),
        Primop::Async { op, .. } => Ok(op(self, rhs).await?),
      },
      v => Err(Error::from(ErrorKind::NotCallable(v.typename()))),
    }
  }

  #[async_recursion]
  async fn call_lambda(
    &self,
    arg: &Spanned<LambdaArg>,
    body: ExprRef,
    rhs: Option<ValueRef>,
    context: &Context,
  ) -> Result<Value> {
    let mut lambda_body_scope = HashMap::new();

    let added_context = match arg.node {
      LambdaArg::Plain(ref arg_name) => {
        lambda_body_scope.insert(arg_name.clone(), rhs.ok_or(ErrorKind::Autocall)?);
        Scope::Static(lambda_body_scope)
      }
      LambdaArg::Formals(ref formals) => {
        let fn_arg_thunk = match rhs {
          Some(id) => {
            self.force(id).await?;
            id
          }
          None => self
            .allocator
            .insert(arg.replace(Value::Attrset(StaticScope::new()))),
        };
        let fn_arg_value = match self.assume_init(fn_arg_thunk)?.node {
          Value::Attrset(h) => h,
          v => return Err(v.expected(Typename::Attrset)),
        };
        let fn_scope_id = self.allocator.insert(arg.replace(Value::blackhole()));

        for lambda_arg in &formals.args {
          let name = &lambda_arg.arg_name;
          match fn_arg_value.get(&name) {
            Some(id) => {
              lambda_body_scope.insert(name.node.clone(), *id);
            }
            None => {
              if let Some(FormalDef { default, .. }) = lambda_arg.fallback {
                let default_arg = self.allocator.insert(Spanned::new(
                  default.span,
                  Value::Thunk {
                    id: default.node,
                    context: context.append(Scope::Dynamic(fn_scope_id)),
                  },
                ));
                lambda_body_scope.insert(name.node.clone(), default_arg);
              } else if rhs.is_some() {
                todo!("Argument {} is missing", name.node);
              } else {
                todo!("Autocall with no default value");
              }
            }
          }
        }

        if let Some(FormalsAt { ref name, .. }) = formals.at {
          lambda_body_scope.insert((**name).clone(), fn_arg_thunk);
        }

        self
          .allocator
          .swap(fn_scope_id, arg.replace(Value::Attrset(lambda_body_scope)));
        Scope::Dynamic(fn_scope_id)
      }
    };

    self
      .step(*body, body.span, &context.append(added_context))
      .await
  }

  async fn lookup_var(&self, id: &Ident, context: &Context) -> Result<Value> {
    for s in context
      .iter()
      .chain(Some(Scope::Dynamic(self.inherent_scope)).iter())
    {
      let this_frame = match s {
        Scope::Static(static0) => static0,
        Scope::Dynamic(id) => self.force_attrs(*id).await?.node,
      };
      if let Some(v) = this_frame.get(id) {
        return Ok(Value::Pointer(*v));
      }
    }
    Err(ErrorKind::UnboundVar(id.to_string()).into())
  }

  async fn build_attrs(
    &self,
    recursive: bool,
    bindings: &[Spanned<Binding>],
    destination: ValueRef,
    context: &Context,
  ) -> Result<()> {
    let mut s = StaticScope::with_capacity(bindings.len());
    let new_scope = context.append(Scope::Dynamic(destination));
    let recursive_scope = if recursive { &new_scope } else { context };

    for b in bindings {
      match &b.node {
        Binding::Plain { path, rhs, .. } => {
          self
            .push_binding(&mut s, &path.0[..], *rhs, &recursive_scope)
            .await?
        }
        Binding::Inherit { from, attrs, .. } => self.push_inherit(&mut s, from.as_ref(), attrs)?,
      }
    }

    self.allocator.swap_deref(destination, Value::Attrset(s));

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
    let span = key1.span;
    let key1 = self.attrname(&*key1, context)?;
    let child_item = if keyrest.is_empty() {
      self.thunk(rhs, context)
    } else {
      let mut next_scope = match scope.get(&key1) {
        Some(i) => self.force_attrs(*i).await?.node.clone(),
        None => HashMap::new(),
      };
      self
        .push_binding(&mut next_scope, keyrest, rhs, context)
        .await?;
      self
        .allocator
        .insert(Spanned::new(span, Value::Attrset(next_scope)))
    };
    scope.insert(key1.into_owned(), child_item);
    Ok(())
  }

  fn push_inherit(
    &self,
    scope: &mut StaticScope,
    from: Option<&InheritFrom>,
    attrs: &AttrList,
  ) -> Result<()> {
    todo!()
  }

  fn attrname<'attr>(&self, a: &'attr AttrName, context: &Context) -> Result<Cow<'attr, Ident>> {
    match a {
      AttrName::Plain(p) => Ok(Cow::Borrowed(p)),
      AttrName::Str { body, .. } => {
        let mut buf = String::new();
        for item in body {
          match item {
            StrPart::Plain(s) => buf.push_str(s),
            StrPart::Quote { quote, .. } => {
              let t = self.thunk(*quote, context);
              todo!()
            }
          }
        }
        Ok(Cow::Owned(buf.into()))
      }
      AttrName::Dynamic { quote, .. } => {
        let value = self.thunk(*quote, context);
        todo!()
      }
    }
  }

  pub fn bail(&self, err: Error) -> ! {
    let msg = err.to_string();
    let diag = Diagnostic::error()
      .with_message(&msg)
      .with_code("EVAL")
      .with_labels(
        err
          .trace
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
            .with_message(if i == 0 {
              "error occurs here"
            } else {
              "while evaluating this expression"
            })
          })
          .collect(),
      );
    codespan_reporting::term::emit(
      &mut self.stderr.lock(),
      &Config {
        display_style: DisplayStyle::Rich,
        ..Default::default()
      },
      &self.source,
      &diag,
    )
    .unwrap();
    std::process::exit(1)
  }
}
