use crate::{
  eval::Eval,
  lock::RwLock,
  prelude::*,
  syntax::{
    expr::{ExprRef, Lambda},
    parse::Located,
  },
};
use std::{
  cell::RefCell,
  collections::{BTreeMap, BTreeSet, HashSet},
  fmt,
  fmt::Debug,
  num::NonZeroU8,
  path::PathBuf,
  rc::Rc,
  sync::Arc,
};

pub type ValueRef = Writable<Value>;
pub type EnvRef = Writable<Env>;
pub type PathSet = BTreeSet<String>;
pub type Attrs = BTreeMap<Ident, Located<ValueRef>>;

#[derive(EnumAsInner, Clone)]
pub enum Value {
  Null,
  Bool(bool),
  Int(i64),
  Float(f64),
  String(Str),
  Path(PathBuf),
  Attrs(Readable<Attrs>),
  List(Readable<Vec<ValueRef>>),
  Apply(ValueRef, ValueRef),
  Thunk(Thunk),
  Lambda(EnvRef, Lambda),
  Primop(Primop, Vec<ValueRef>),
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Thunk {
  #[derivative(Debug = "ignore")]
  pub env: EnvRef,
  pub expr: ExprRef,
}

impl Clone for Thunk {
  fn clone(&self) -> Self {
    panic!("thunks may not be cloned")
  }
}

#[derive(Debug, Clone)]
pub struct Str {
  pub s: String,
  pub context: PathSet,
}

impl Value {
  pub fn typename(&self) -> String {
    match self {
      Value::Null => "null".to_string(),
      Value::Bool { .. } => "bool".to_string(),
      Value::Int { .. } => "int".to_string(),
      Value::Float { .. } => "float".to_string(),
      Value::String { .. } => "string".to_string(),
      Value::Path { .. } => "path".to_string(),
      Value::Attrs { .. } => "attrset".to_string(),
      Value::List { .. } => "list".to_string(),
      Value::Lambda { .. } => "lambda".to_string(),
      Value::Primop(_, args) => {
        if args.is_empty() {
          "primop".to_string()
        } else {
          "primop-app".to_string()
        }
      }
      Value::Thunk(Thunk { expr, .. }) => format!("thunk @ {}", expr),
      Value::Apply(lhs, rhs) => {
        format!("App({:?}\n  {:?})", lhs.read().debug(), rhs.read().debug())
      }
    }
  }

  pub fn thunk(env: EnvRef, expr: ExprRef) -> Self {
    Self::Thunk(Thunk { env, expr })
  }

  pub fn string_bare<S: Into<String>>(s: S) -> Self {
    Self::String(Str {
      s: s.into(),
      context: Default::default(),
    })
  }

  #[allow(clippy::needless_lifetimes)] // false positive
  pub fn debug<'a>(&'a self) -> impl Debug + 'a {
    DebugValue {
      value: self,
      seen: Default::default(),
    }
  }
}

pub struct Env {
  pub prev_with: Option<NonZeroU8>,
  pub values: Vec<ValueRef>,
  pub env_type: EnvType,
  pub up: Option<EnvRef>,
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum EnvType {
  Plain,
  HasWithExpr,
  HasWithAttrs,
}

impl Env {
  pub fn climb(e: EnvRef, l: NonZeroU8) -> EnvRef {
    Self::climb_impl(e, l.get())
  }

  fn climb_impl(e: EnvRef, l: u8) -> EnvRef {
    if l == 0 {
      e
    } else {
      Self::climb_impl(
        e.read()
          .up
          .as_ref()
          .expect("incorrect offset in level()")
          .clone(),
        l - 1,
      )
    }
  }

  #[allow(clippy::needless_lifetimes)]
  pub fn debug<'e>(&'e self) -> impl Debug + 'e {
    "<env>"
    // DebugEnv {
    //   env: self,
    //   seen: Default::default(),
    // }
  }
}

impl Default for Env {
  fn default() -> Self {
    Self {
      up: None,
      prev_with: None,
      values: vec![],
      env_type: EnvType::HasWithAttrs,
    }
  }
}

pub trait PrimopFn = Fn(&Eval, Pos, Vec<ValueRef>) -> Result<Value>;

#[derive(Clone)]
pub struct Primop {
  pub fun: Readable<dyn PrimopFn>,
  pub name: Ident,
  pub arity: u8,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "<primop {}>", self.name)
  }
}

type PtrSet<T> = Rc<RefCell<HashSet<*const T>>>;

struct DebugValue<'v> {
  value: &'v Value,
  seen: PtrSet<RwLock<Value>>,
}

impl<'v> Debug for DebugValue<'v> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    macro_rules! dup {
      ($formatter:ident . $name:ident ( $v:expr )) => {
        if self.seen.borrow_mut().insert(Arc::as_ptr(&$v)) {
          $formatter.$name(&DebugValue {
            value: &*$v.read(),
            seen: Rc::clone(&self.seen),
          });
        } else {
          $formatter.$name(&"<repeated>");
        }
      };
    }

    match self.value {
      Value::Null => f.debug_tuple("Null").finish(),
      Value::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
      Value::Int(i) => f.debug_tuple("Int").field(i).finish(),
      Value::Float(i) => f.debug_tuple("Float").field(i).finish(),
      Value::String(Str { s, .. }) => f.debug_tuple("String").field(s).finish(),
      Value::Path(p) => f.debug_tuple("Path").field(p).finish(),
      Value::Attrs(attrs) => {
        let mut x = f.debug_map();
        for (k, v) in attrs.iter() {
          x.key(k);
          dup!(x.value(v.v));
        }
        x.finish()
      }
      Value::List(l) => DebugValueList {
        values: l.as_slice(),
        seen: Rc::clone(&self.seen),
      }
      .fmt(f),
      Value::Apply(l, r) => {
        let mut x = f.debug_tuple("Apply");
        dup!(x.field(l));
        dup!(x.field(r));
        x.finish()
      }
      Value::Thunk(t) => f.debug_tuple("Thunk").field(t).finish(),
      Value::Lambda(e, l) => f
        .debug_tuple("Lambda")
        .field(&DebugEnv {
          env: &*e.read(),
          seen: Rc::clone(&self.seen),
        })
        .field(l)
        .finish(),
      Value::Primop(p, v) => f
        .debug_tuple("Primop")
        .field(p)
        .field(&DebugValueList {
          values: v.as_slice(),
          seen: Rc::clone(&self.seen),
        })
        .finish(),
    }
  }
}

struct DebugEnv<'e> {
  env: &'e Env,
  seen: PtrSet<RwLock<Value>>,
}

impl<'e> Debug for DebugEnv<'e> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut e = f.debug_struct("Env");
    e.field("prev_with", &self.env.prev_with)
      .field(
        "values",
        &DebugValueList {
          values: self.env.values.as_slice(),
          seen: Rc::clone(&self.seen),
        },
      )
      .field("env_type", &self.env.env_type);
    e.field(
      "up",
      &self.env.up.as_ref().map(|x| DebugEnvRef {
        env: x,
        seen: Rc::clone(&self.seen),
      }),
    )
    .finish()
  }
}

struct DebugEnvRef<'e> {
  env: &'e EnvRef,
  seen: PtrSet<RwLock<Value>>,
}

impl<'e> Debug for DebugEnvRef<'e> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    DebugEnv {
      env: &*self.env.read(),
      seen: Rc::clone(&self.seen),
    }
    .fmt(f)
  }
}

struct DebugValueList<'v> {
  values: &'v [ValueRef],
  seen: PtrSet<RwLock<Value>>,
}

impl<'v> Debug for DebugValueList<'v> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut x = f.debug_list();
    for item in self.values {
      x.entry(&DebugValue {
        value: &*item.read(),
        seen: Rc::clone(&self.seen),
      });
    }
    x.finish()
  }
}
