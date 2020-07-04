#[macro_use] extern crate log;

mod builtins;
pub mod error;
mod scope;
pub mod source_tree;
pub mod value;

use arena::Arena;
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
use std::{collections::HashMap, path::Path};
use syntax::{
  expr::{self, Expr, ExprRef},
  span::{FileSpan, Spanned},
};
use value::{Typename, Value, ValueRef};

pub type Result<T> = anyhow::Result<T, error::Error>;

pub struct Eval {
  allocator: Arena<Spanned<Value>>,
  source: Source,
  frame_context: Context,
  inherent_scope: ValueRef,
  inline_counter: usize,
  stderr: StandardStream,
}

macro_rules! mk_cast {
  ($f:ident, $output:ty, $expected:ident, $($p:pat => $v:expr),+) => {
    fn $f(&mut self, v: ValueRef) -> Result<Spanned<$output>> {
      let Spanned { span, node } = self.forced(v)?;
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

  mk_cast!(force_path_like, &Path, Unknown, Value::String { s, .. } => Path::new(s));

  pub fn new() -> Result<Self> {
    let mut this = Self {
      allocator: Arena::new(),
      source: Source::new(),
      frame_context: Context::new(),
      inline_counter: 0,
      stderr: StandardStream::stderr(ColorChoice::Auto),
      inherent_scope: unsafe { std::mem::zeroed() }, // never read
    };
    let builtins_id = this.source.files.add("builtins.nix", "<...>".into());
    let builtins_span = FileSpan::new(0u32, 0u32, builtins_id);
    let id = builtins::load_inherent_scope(&mut this, builtins_span)?;
    this.inherent_scope = id;
    Ok(this)
  }

  pub fn source(&self) -> &Source {
    &self.source
  }

  pub async fn load<P: AsRef<Path>>(&mut self, file: P) -> Result<ValueRef> {
    let ref0 = self.source.load_file(file).await?;
    Ok(self.allocator.insert(Spanned::new(
      ref0.span,
      Value::Thunk {
        id: ref0.node,
        context: Context::new(),
      },
    )))
  }

  pub fn load_inline<S: AsRef<str>>(&mut self, code: S) -> Result<ValueRef> {
    let fakename = format!("<inline-{}>", self.inline_counter);
    self.inline_counter += 1;
    let ref0 = self.source.load_inline(fakename, code.as_ref())?;
    Ok(self.allocator.insert(Spanned::new(
      ref0.span,
      Value::Thunk {
        id: ref0.node,
        context: Context::new(),
      },
    )))
  }

  fn thunk_here(&self, t: ExprRef) -> ValueRef {
    self.allocator.insert(Spanned::new(
      t.span,
      Value::Thunk {
        id: t.node,
        context: self.frame_context.clone(),
      },
    ))
  }

  pub fn force(&mut self, id: ValueRef) -> Result<()> {
    loop {
      let span = self.allocator[id].span;
      if let Value::Blackhole = self.allocator[id].node {
        return Err(Error::from(ErrorKind::Loop));
      } else if let Value::Pointer(p) = self.allocator[id].node {
        break self.force(p);
      } else if let Value::App { lhs, rhs } = self.allocator[id].node {
        *self.allocator[id] = Value::Blackhole;
        *self.allocator[id] = self
          .call_function(lhs, rhs)
          .map_err(|e| e.add_frame(span))?;
      } else if let Some((context, expr_id)) = self.allocator[id].node.take_thunk() {
        let value = self.with_context(context, |e| e.step(expr_id, span))?;
        *self.allocator[id] = value;
      } else {
        break Ok(());
      }
    }
  }

  fn step(&mut self, id: ExprId, at: FileSpan) -> Result<Value> {
    self._do_step(id, at).map_err(|e| e.add_frame(at))
  }

  fn _do_step(&mut self, id: ExprId, span: FileSpan) -> Result<Value> {
    match *self.source.expr(id) {
      Expr::Pos => {}
      Expr::Int(i) => return Ok(Value::Int(i)),
      Expr::Float(f) => return Ok(Value::Float(f)),
      Expr::Var(ref ident) => {
        let ident = ident.clone();
        return self.lookup_var(ident);
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
              s: path.into(),
              context: Default::default(),
            },
          ));
          let nixpath = self
            .force_attrs(self.inherent_scope)?
            .get(&Ident::from("nixPath"))
            .copied()
            .ok_or_else(|| ErrorKind::Unimplemented("attribute nixPath does not exist".into()))?;
          return Ok(Value::Path(builtins::find_file(self, nixpath, strlit)?));
        }
      },
      Expr::Uri(_) => {}
      Expr::Lambda(ref l) => {
        return Ok(Value::Lambda {
          arg: l.argument.clone(),
          body: l.body,
          captures: self.frame_context.clone(),
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
          lhs: self.thunk_here(lhs),
          rhs: self.thunk_here(rhs),
        })
      }
      Expr::Select(_) => {}
      Expr::AttrSet(AttrSet { rec, ref attrs, .. }) => {
        let new_attrset = self.allocator.insert(Spanned::new(span, Value::Blackhole));
        let attrs = attrs.clone();
        self.build_attrs(rec.is_some(), attrs, new_attrset)?;
        return Ok(Value::Pointer(new_attrset));
      }
    }

    todo!("{:?}", self.source().expr(id))
  }

  pub fn forced(&mut self, id: ValueRef) -> Result<Spanned<&Value>> {
    self.force(id)?;

    self.assume_init(id)
  }

  fn assume_init(&self, mut id: ValueRef) -> Result<Spanned<&Value>> {
    loop {
      let val = &self.allocator[id];
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

  fn with_context<T, F: FnOnce(&mut Self) -> T>(&mut self, ctx: Context, f: F) -> T {
    let old_ctx = std::mem::replace(&mut self.frame_context, ctx);
    let result = f(self);
    self.frame_context = old_ctx;
    result
  }

  fn with_more_context<T, F: FnOnce(&mut Self) -> T>(&mut self, s: Scope, f: F) -> T {
    self.with_context(self.frame_context.append(s), f)
  }

  fn call_function(&mut self, lhs: ValueRef, rhs: ValueRef) -> Result<Value> {
    match self.forced(lhs)?.node {
      Value::Lambda {
        arg,
        body,
        captures,
      } => {
        let arg = arg.clone();
        let lam_caps = captures.clone();
        let body = *body;
        self.with_context(lam_caps, |e| e.call_lambda(arg, body, Some(rhs)))
      }
      Value::Primop(p) => Ok(p.clone().call(self, rhs)?),
      v => Err(Error::from(ErrorKind::NotCallable(v.typename()))),
    }
  }

  fn call_lambda(
    &mut self,
    arg: Spanned<LambdaArg>,
    body: ExprRef,
    rhs: Option<ValueRef>,
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
            self.force(id)?;
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
        let fn_scope_id = self.allocator.insert(arg.replace(Value::Blackhole));

        for lambda_arg in &formals.args {
          let name = lambda_arg.arg_name.clone();
          match fn_arg_value.get(&name) {
            Some(id) => {
              lambda_body_scope.insert(name.node, *id);
            }
            None => {
              if let Some(FormalDef { default, .. }) = lambda_arg.fallback {
                let default_arg = self.allocator.insert(Spanned::new(
                  default.span,
                  Value::Thunk {
                    id: default.node,
                    context: self.frame_context.append(Scope::Dynamic(fn_scope_id)),
                  },
                ));
                lambda_body_scope.insert(name.node, default_arg);
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

        *self.allocator[fn_scope_id] = Value::Attrset(lambda_body_scope);
        Scope::Dynamic(fn_scope_id)
      }
    };

    self.with_more_context(added_context, |eval| eval.step(*body, body.span))
  }

  fn lookup_var(&mut self, id: Ident) -> Result<Value> {
    for s in self
      .frame_context
      .clone()
      .iter()
      .chain(Some(Scope::Dynamic(self.inherent_scope)).iter())
    {
      let this_frame = match s {
        Scope::Static(static0) => static0,
        Scope::Dynamic(id) => self.force_attrs(*id)?.node,
      };
      if let Some(v) = this_frame.get(&id) {
        return Ok(Value::Pointer(*v));
      }
    }
    Err(ErrorKind::UnboundVar(id.to_string()).into())
  }

  fn build_attrs(
    &mut self,
    recursive: bool,
    bindings: Vec<Spanned<Binding>>,
    destination: ValueRef,
  ) -> Result<()> {
    let mut s = StaticScope::with_capacity(bindings.len());
    let original_scope = self.frame_context.clone();
    let recursive_scope = if recursive {
      self.frame_context.append(Scope::Dynamic(destination))
    } else {
      original_scope.clone()
    };

    for b in bindings {
      match b.node {
        Binding::Plain { path, rhs, .. } => self.with_context(recursive_scope.clone(), |e| {
          e.push_binding(&mut s, &path.0[..], rhs)
        })?,
        Binding::Inherit { from, attrs, .. } => self.push_inherit(&mut s, from, attrs)?,
      }
    }

    *self.allocator[destination] = Value::Attrset(s);

    self.frame_context = original_scope;

    Ok(())
  }

  fn push_binding(
    &mut self,
    scope: &mut StaticScope,
    names: &[Spanned<AttrName>],
    rhs: ExprRef,
  ) -> Result<()> {
    let (key1, keyrest) = names.split_first().unwrap();
    let span = key1.span;
    let key1 = self.attrname(&*key1)?;
    let child_item = if keyrest.is_empty() {
      self.thunk_here(rhs)
    } else {
      let mut next_scope = match scope.get(&key1) {
        Some(i) => todo!("self.force_attrs(*i)?.node.clone()"),
        None => HashMap::new(),
      };
      self.push_binding(&mut next_scope, keyrest, rhs)?;
      self
        .allocator
        .insert(Spanned::new(span, Value::Attrset(next_scope)))
    };
    scope.insert(key1, child_item);
    Ok(())
  }

  fn push_inherit(
    &mut self,
    scope: &mut StaticScope,
    from: Option<InheritFrom>,
    attrs: AttrList,
  ) -> Result<()> {
    todo!()
  }

  fn attrname(&mut self, a: &AttrName) -> Result<Ident> {
    match a {
      AttrName::Plain(p) => Ok(p.clone()),
      AttrName::Str { body, .. } => {
        let mut buf = String::new();
        for item in body {
          match item {
            StrPart::Plain(s) => buf.push_str(s),
            StrPart::Quote { quote, .. } => {
              let t = self.thunk_here(*quote);
              todo!()
            }
          }
        }
        Ok(buf.into())
      }
      AttrName::Dynamic { quote, .. } => {
        let value = self.thunk_here(*quote);
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
      self.source(),
      &diag,
    )
    .unwrap();
    std::process::exit(1)
  }
}
