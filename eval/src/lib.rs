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
use expr::{ExprId, Ident, LambdaArg};
use scope::{Context, Scope, StaticScope};
use source_tree::Source;
use std::{collections::HashMap, path::Path};
use syntax::{
  expr::{self, Expr, ExprRef},
  span::{FileSpan, Spanned},
};
use value::{Value, ValueRef};

type Result<T> = anyhow::Result<T, error::Error>;

pub struct Eval {
  allocator: Arena<Spanned<Value>>,
  source: Source,
  context: Context,
  inline_counter: usize,
  stderr: StandardStream,
}

impl Eval {
  pub fn new() -> Self {
    Self {
      allocator: Arena::new(),
      source: Source::new(),
      context: Context::new(),
      inline_counter: 0,
      stderr: StandardStream::stderr(ColorChoice::Auto),
    }
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
        context: self.context.clone(),
      },
    ))
  }

  pub fn force(&mut self, id: ValueRef) -> Result<()> {
    loop {
      let span = self.allocator[id].span;
      if let Value::Blackhole = self.allocator[id].node {
        panic!("Blackhole");
      } else if let Value::Pointer(p) = self.allocator[id].node {
        break self.force(p);
      } else if let Value::App { lhs, rhs } = self.allocator[id].node {
        *self.allocator[id] = Value::Blackhole;
        *self.allocator[id] = self
          .call_function(lhs, rhs)
          .map_err(|e| e.add_frame(span))?;
      } else if let Value::Thunk {
        id: expr_id,
        ref context,
      } = self.allocator[id].node
      {
        let ctx = context.clone();
        *self.allocator[id] = Value::Blackhole;
        let value = self.with_context(ctx, |e| e.step(expr_id, span))?;
        *self.allocator[id] = value;
      } else {
        break Ok(());
      }
    }
  }

  fn step(&mut self, id: ExprId, at: FileSpan) -> Result<Value> {
    self._do_step(id, at).map_err(|e| e.add_frame(at))
  }

  fn _do_step(&self, id: ExprId, _: FileSpan) -> Result<Value> {
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
      Expr::Path(_) => {}
      Expr::Uri(_) => {}
      Expr::Lambda(ref l) => {
        return Ok(Value::Lambda {
          arg: l.argument.clone(),
          body: l.body,
          captures: self.context.clone(),
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
      Expr::AttrSet(_) => {}
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
    let old_ctx = std::mem::replace(&mut self.context, ctx);
    let result = f(self);
    self.context = old_ctx;
    result
  }

  fn with_more_context<T, F: FnOnce(&mut Self) -> T>(&mut self, s: Scope, f: F) -> T {
    self.with_context(self.context.append(s), f)
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
      v => {
        Err(Error::from(ErrorKind::NotCallable(v.typename())).add_frame(self.allocator[lhs].span))
      }
    }
  }

  fn call_lambda(
    &mut self,
    arg: Spanned<LambdaArg>,
    body: ExprRef,
    rhs: Option<ValueRef>,
  ) -> Result<Value> {
    let mut lambda_body_scope = HashMap::new();

    match arg.node {
      LambdaArg::Plain(ref arg_name) => {
        lambda_body_scope.insert(arg_name.clone(), rhs.ok_or(ErrorKind::Autocall)?);
      }
      LambdaArg::Formals(ref formals) => {
        let fn_arg_thunk = match rhs {
          Some(id) => id,
          None => self
            .allocator
            .insert(arg.replace(Value::Attrset(StaticScope::new()))),
        };
        self.force(fn_arg_thunk)?;
        let fn_arg_value = match self.assume_init(fn_arg_thunk)?.node {
          Value::Attrset(h) => h,
          v => return Err(ErrorKind::Unimplemented("Wrong type".into()).into()),
        };
        let fn_scope_id = self.allocator.insert(arg.replace(Value::Blackhole));

        for lambda_arg in &formals.args {
          let name = lambda_arg.arg_name.clone();
          match fn_arg_value.get(&name) {
            Some(_) => {}
            None => {}
          }
        }
      }
    }

    self.with_more_context(Scope::Static(lambda_body_scope), |eval| {
      eval.step(*body, body.span)
    })
  }

  fn lookup_var(&self, id: Ident) -> Result<Value> {
    for s in &self.context {
      let this_frame = match s {
        Scope::Static(static0) => static0,
        Scope::Dynamic(_) => todo!("dynamic scope"),
      };
      if let Some(v) = this_frame.get(&id) {
        return Ok(Value::Pointer(*v));
      }
    }
    Err(ErrorKind::UnboundVar(id.to_string()).into())
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

impl Default for Eval {
  fn default() -> Self {
    Self::new()
  }
}
