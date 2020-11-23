use super::{
  parse::{Located, Pos},
  UserError,
};
use crate::prelude::*;
use std::{
  collections::HashMap,
  fmt,
  fmt::Display,
  path::PathBuf,
  sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
  },
};

pub type ExprRef = Readable<Expr>;

type ParseOk = Result<(), Located<UserError>>;

#[derive(Debug, EnumAsInner)]
pub enum Expr {
  Pos {
    pos: Pos,
  },
  Var(Writable<Var>),
  Int {
    n: i64,
  },
  Float {
    f: f64,
  },
  String {
    s: String,
  },
  Path {
    path: PathBuf,
  },
  Select {
    pos: Pos,
    lhs: ExprRef,
    def: Option<ExprRef>,
    path: AttrPath,
  },
  Lambda(Lambda),
  List(Vec<Expr>),
  Attrs(Attrs),
  Assert {
    pos: Pos,
    cond: ExprRef,
    body: ExprRef,
  },
  With {
    pos: Pos,
    env: ExprRef,
    body: ExprRef,
    prev_with: AtomicUsize,
  },
  Let {
    attrs: Attrs,
    body: ExprRef,
  },
  ConcatStrings {
    pos: Pos,
    force_string: bool,
    parts: Vec<Expr>,
  },
  If {
    pos: Pos,
    cond: ExprRef,
    rhs1: ExprRef,
    rhs2: ExprRef,
  },
  Op {
    bin: Bin,
    pos: Pos,
    lhs: ExprRef,
    rhs: ExprRef,
  },
  Apply {
    pos: Pos,
    lhs: ExprRef,
    rhs: ExprRef,
  },
  HasAttr {
    lhs: ExprRef,
    path: AttrPath,
  },
  Not(ExprRef),
}

pub fn swap<A, B>(x: Located<(A, B)>) -> Located<(B, A)> {
  Located {
    pos: x.pos,
    v: (x.v.1, x.v.0),
  }
}

impl Expr {
  pub fn bind_vars(&self, env: &StaticEnvRef) -> ParseOk {
    match self {
      Expr::Pos { .. }
      | Expr::Int { .. }
      | Expr::Float { .. }
      | Expr::String { .. }
      | Expr::Path { .. } => {}
      Expr::Var(v) => v.write().bind_vars(env.clone())?,
      Expr::Select { lhs, def, path, .. } => {
        lhs.bind_vars(env)?;
        def.as_ref().map(|x| x.bind_vars(env)).transpose()?;
        for attr in path {
          attr.as_dynamic().map(|x| x.bind_vars(env)).transpose()?;
        }
      }
      Expr::HasAttr { lhs, path } => {
        lhs.bind_vars(env)?;
        for attr in path {
          attr.as_dynamic().map(|x| x.bind_vars(env)).transpose()?;
        }
      }
      Expr::Attrs(a) => a.bind_vars(env.clone())?,
      Expr::Lambda(l) => l.bind_vars(env)?,
      Expr::List(l) => {
        for thing in l {
          thing.bind_vars(env)?;
        }
      }
      Expr::Assert { cond, body, .. } => {
        cond.bind_vars(env)?;
        body.bind_vars(env)?;
      }
      Expr::With {
        env: attrs,
        body,
        prev_with,
        ..
      } => {
        let mut cur_env = env.clone();

        for level in 1.. {
          let ce = cur_env.read();
          if ce.is_with {
            prev_with.store(level, Ordering::Release);
            break;
          }

          match ce.up {
            Some(ref x) => {
              let new_thing = x.clone();
              drop(ce);
              cur_env = new_thing;
            }
            None => break,
          }
        }

        attrs.bind_vars(env)?;
        let new_env = writable(StaticEnv {
          up: Some(env.clone()),
          is_with: true,
          vars: Default::default(),
        });
        body.bind_vars(&new_env)?;
      }
      Expr::Let { attrs, body } => {
        let new_env = writable(StaticEnv {
          is_with: false,
          up: Some(env.clone()),
          vars: Default::default(),
        });

        for (i, (k, v)) in attrs.attrs.iter().enumerate() {
          v.displ.fetch_add(1, Ordering::Acquire);
          new_env.write().vars.insert(k.clone(), i);
        }

        for v in attrs.attrs.values() {
          v.rhs.bind_vars(if v.inherited { &env } else { &new_env })?;
        }

        body.bind_vars(&new_env)?;
      }
      Expr::ConcatStrings { parts, .. } => {
        for part in parts {
          part.bind_vars(env)?;
        }
      }
      Expr::If {
        cond, rhs1, rhs2, ..
      } => {
        cond.bind_vars(env)?;
        rhs1.bind_vars(env)?;
        rhs2.bind_vars(env)?;
      }
      Expr::Op { lhs, rhs, .. } => {
        lhs.bind_vars(env)?;
        rhs.bind_vars(env)?;
      }
      Expr::Apply { lhs, rhs, .. } => {
        lhs.bind_vars(env)?;
        rhs.bind_vars(env)?;
      }
      Expr::Not(e) => e.bind_vars(env)?,
    }
    Ok(())
  }

  pub fn bin(operator: Bin, args: Located<(Self, Self)>) -> Self {
    Self::Op {
      bin: operator,
      pos: args.pos,
      lhs: readable(args.v.0),
      rhs: readable(args.v.1),
    }
  }

  pub fn lt(args: Located<(Self, Self)>, invert: bool) -> Self {
    let inner = Self::apply(Located {
      pos: args.pos,
      v: (
        Self::apply(Located {
          pos: args.pos,
          v: (Self::Var(Var::new(args.pos, "__lessThan".into())), args.v.0),
        }),
        args.v.1,
      ),
    });
    if invert {
      Self::Not(readable(inner))
    } else {
      inner
    }
  }

  pub fn apply(args: Located<(Self, Self)>) -> Self {
    Self::Apply {
      pos: args.pos,
      lhs: readable(args.v.0),
      rhs: readable(args.v.1),
    }
  }

  pub fn string<S: Into<String>>(s: S) -> Self {
    let s = s.into();
    Self::String { s }
  }

  pub fn path<P: Into<PathBuf>>(p: P) -> Self {
    let p = p.into();
    Self::Path { path: p }
  }
}

fn show_attrpath(f: &mut fmt::Formatter, path: &[AttrName]) -> fmt::Result {
  for (i, attr) in path.iter().enumerate() {
    if i > 0 {
      f.write_str(".")?;
    }
    match attr {
      AttrName::Static(k) => k.fmt(f)?,
      AttrName::Dynamic(e) => write!(f, "\"${{{}}}\"", e)?,
    }
  }
  Ok(())
}

#[derive(Debug, Display, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Bin {
  #[display(fmt = "==")]
  Eq,
  #[display(fmt = "!=")]
  Neq,
  #[display(fmt = "&&")]
  And,
  #[display(fmt = "||")]
  Or,
  #[display(fmt = "->")]
  Impl,
  #[display(fmt = "//")]
  Update,
  #[display(fmt = "++")]
  ConcatLists,
  #[display(fmt = "+")]
  Add,
  #[display(fmt = "-")]
  Sub,
  #[display(fmt = "*")]
  Mul,
  #[display(fmt = "/")]
  Div,
}

pub type AttrPath = Vec<AttrName>;
pub type AttrList = Vec<AttrName>;

#[derive(Debug, EnumAsInner)]
pub enum AttrName {
  Static(Ident),
  Dynamic(ExprRef),
}

#[derive(Debug)]
pub struct Var {
  pub pos: Pos,
  pub name: Ident,
  pub displ: Displacement,
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub enum Displacement {
  Static { level: usize, offset: usize },
  Dynamic { level: usize },
  Unknown,
}

impl Displacement {
  pub fn level(self) -> Option<usize> {
    match self {
      Self::Static { level, .. } | Self::Dynamic { level } => Some(level),
      Self::Unknown => None,
    }
  }
}

impl Var {
  pub fn new(pos: Pos, name: Ident) -> Writable<Self> {
    writable(Self {
      pos,
      name,
      displ: Displacement::Unknown,
    })
  }

  pub fn bind_vars(&mut self, mut env: StaticEnvRef) -> ParseOk {
    let mut with_level = None;

    for level in 0.. {
      let guard = env.read();

      if guard.is_with {
        if with_level.is_none() {
          with_level = Some(level);
        }
      } else if let Some(displ) = guard.vars.get(&self.name) {
        trace!(
          "{} resolves to {} @ {}, within {:?}",
          self.name,
          displ,
          level,
          guard.vars
        );
        self.displ = Displacement::Static {
          level,
          offset: *displ,
        };
        return Ok(());
      }

      match guard.up {
        Some(ref x) => {
          let new = x.clone();
          drop(guard);
          env = new;
        }
        None => break,
      }
    }

    match with_level {
      None => Err(Located {
        pos: self.pos,
        v: UserError::UndefinedVariable(self.name.clone()),
      }),
      Some(w) => {
        self.displ = Displacement::Dynamic { level: w };
        Ok(())
      }
    }
  }
}

#[derive(Debug)]
pub enum LambdaArg {
  Plain(Ident),
  Formals {
    name: Option<Ident>,
    formals: Formals,
  },
}

#[derive(Debug)]
pub struct Formals {
  pub formals: Vec<Formal>,
  pub ellipsis: bool,
}

#[derive(Debug)]
pub struct Formal {
  pub pos: Pos,
  pub name: Ident,
  pub def: Option<ExprRef>,
}

#[derive(Debug)]
pub enum ParseBinding {
  Plain(AttrPath, Expr),
  Inherit(AttrList),
  InheritFrom(Expr, AttrList),
}

#[derive(Debug)]
pub struct Lambda {
  pub pos: Pos,
  pub name: Ident,
  pub arg: LambdaArg,
  pub body: ExprRef,
}

impl Lambda {
  fn bind_vars(&self, env: &StaticEnvRef) -> ParseOk {
    let new_env = writable(StaticEnv {
      up: Some(env.clone()),
      is_with: false,
      vars: Default::default(),
    });

    let mut displ = 0;

    let arg = match &self.arg {
      LambdaArg::Plain(n) => Some(n),
      LambdaArg::Formals { name, .. } => name.as_ref(),
    };

    if let Some(a) = arg {
      new_env.write().vars.insert(a.clone(), displ);
      displ += 1;
    }

    if let LambdaArg::Formals { ref formals, .. } = self.arg {
      for f in &formals.formals {
        new_env.write().vars.insert(f.name.clone(), displ);
        displ += 1;
      }

      for f in &formals.formals {
        if let Some(d) = &f.def {
          d.bind_vars(&new_env)?;
        }
      }
    }

    self.body.bind_vars(&new_env)
  }
}

#[derive(Debug, Default)]
pub struct Attrs {
  pub recursive: bool,
  pub attrs: HashMap<Ident, AttrDef>,
  pub dyn_attrs: Vec<DynAttrDef>,
}

impl Attrs {
  pub fn collect<I: IntoIterator<Item = Located<ParseBinding>>>(
    pos: Pos,
    bindings: I,
    allow_dyn: bool,
  ) -> Result<Self, Located<UserError>> {
    let mut this = Self::default();
    for item in bindings {
      match item.v {
        ParseBinding::Plain(path, rhs) => {
          this.add_attr(item.pos, &*path, rhs)?;
        }
        ParseBinding::Inherit(_) => {}
        ParseBinding::InheritFrom(_, _) => {}
      }
    }
    if !allow_dyn && !this.dyn_attrs.is_empty() {
      Err(Located {
        pos,
        v: UserError::Other("dynamic attributes not allowed here".into()),
      })
    } else {
      Ok(this)
    }
  }

  fn bind_vars(&self, env: StaticEnvRef) -> ParseOk {
    let mut dynamic_env = &env;
    let new_env = writable(StaticEnv {
      is_with: false,
      up: Some(env.clone()),
      vars: Default::default(),
    });

    if self.recursive {
      dynamic_env = &new_env;

      for (i, (name, value)) in self.attrs.iter().enumerate() {
        value.displ.store(i, Ordering::Release);
        new_env.write().vars.insert(name.clone(), i);
      }

      for val in self.attrs.values() {
        val
          .rhs
          .bind_vars(if val.inherited { &env } else { &new_env })?;
      }
    } else {
      for val in self.attrs.values() {
        val.rhs.bind_vars(&env)?;
      }
    }

    for val in &self.dyn_attrs {
      val.name.bind_vars(&dynamic_env)?;
      val.value.bind_vars(&dynamic_env)?;
    }

    Ok(())
  }

  fn add_attr(&mut self, pos: Pos, path: &[AttrName], rhs: Expr) -> ParseOk {
    match path {
      [] => panic!("invariant violation: empty attrpath produced by parser"),
      [AttrName::Static(i)] => {
        if let Some(x) = self.attrs.get_mut(i) {
          if let (Expr::Attrs(ae), Some(j)) =
            (rhs, Arc::get_mut(&mut x.rhs).unwrap().as_attrs_mut())
          {
            for (ad, av) in ae.attrs {
              if j.attrs.insert(ad, av).is_some() {
                panic!("duplicate attribute");
              }
            }
          } else {
            panic!("duplicate attribute")
          }
        } else {
          self.attrs.insert(
            i.clone(),
            AttrDef {
              pos,
              inherited: false,
              rhs: readable(rhs),
              displ: AtomicUsize::new(0),
            },
          );
        }
      }
      [AttrName::Dynamic(e)] => self.dyn_attrs.push(DynAttrDef {
        pos,
        name: e.clone(),
        value: readable(rhs),
      }),
      [AttrName::Static(i), rest @ ..] => {
        if let Some(j) = self.attrs.get_mut(i) {
          if !j.inherited {
            return Arc::get_mut(&mut j.rhs)
              .unwrap()
              .as_attrs_mut()
              .expect("duplicate attribute")
              .add_attr(pos, rest, rhs);
          } else {
            panic!("duplicate attribute")
          }
        } else {
          let mut new_attrs = Attrs::default();
          new_attrs.add_attr(pos, rest, rhs)?;
          self.attrs.insert(
            i.clone(),
            AttrDef {
              pos,
              inherited: false,
              rhs: readable(Expr::Attrs(new_attrs)),
              displ: AtomicUsize::new(0),
            },
          );
        }
      }
      [AttrName::Dynamic(e), rest @ ..] => {
        let mut new_attrs = Attrs::default();
        new_attrs.add_attr(pos, rest, rhs)?;
        self.dyn_attrs.push(DynAttrDef {
          pos,
          name: e.clone(),
          value: readable(Expr::Attrs(new_attrs)),
        });
      }
    }
    Ok(())
  }
}

#[derive(Debug)]
pub struct AttrDef {
  pub pos: Pos,
  pub inherited: bool,
  pub rhs: ExprRef,
  pub displ: AtomicUsize,
}

#[derive(Debug)]
pub struct DynAttrDef {
  pub pos: Pos,
  pub name: ExprRef,
  pub value: ExprRef,
}

impl Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Expr::Pos { .. } => write!(f, "__curPos"),
      Expr::Var(n) => n.read().name.fmt(f),
      Expr::Int { n, .. } => n.fmt(f),
      Expr::Float { f: g, .. } => g.fmt(f),
      Expr::String { s, .. } => write!(f, "{:?}", s),
      Expr::Path { path, .. } => path.display().fmt(f),
      Expr::Select { lhs, def, path, .. } => {
        write!(f, "({}).", lhs)?;
        show_attrpath(f, path)?;
        if let Some(x) = def {
          write!(f, " or ({})", x)?;
        }
        Ok(())
      }
      Expr::Lambda(Lambda { body, arg, .. }) => {
        write!(f, "(")?;
        match arg {
          LambdaArg::Plain(i) => write!(f, "{}", i)?,
          LambdaArg::Formals {
            name: Some(x),
            formals,
          } => write!(f, "{} @ {}", formals, x)?,
          LambdaArg::Formals {
            name: None,
            formals,
          } => write!(f, "{}", formals)?,
        }
        write!(f, ": {}", body)?;
        write!(f, ")")
      }
      Expr::List(l) => {
        write!(f, "[ ")?;
        for item in l {
          write!(f, "({}) ", item)?;
        }
        f.write_str("]")
      }
      Expr::Attrs(a) => {
        if a.recursive {
          write!(f, "rec ")?;
        }
        write!(f, "{{{}}}", a)
      }
      Expr::Assert { cond, body, .. } => write!(f, "assert {}; {}", cond, body),
      Expr::With { env, body, .. } => write!(f, "(with {}; {})", env, body),
      Expr::Let { attrs, body } => write!(f, "(let {} in {})", attrs, body),
      Expr::ConcatStrings { parts, .. } => {
        write!(f, "(")?;
        for (i, elem) in parts.iter().enumerate() {
          if i > 0 {
            write!(f, " + ")?;
          }
          elem.fmt(f)?;
        }
        write!(f, ")")
      }
      Expr::If {
        cond, rhs1, rhs2, ..
      } => write!(f, "(if {} then {} else {})", cond, rhs1, rhs2),
      Expr::Apply { lhs, rhs, .. } => write!(f, "({} {})", lhs, rhs),
      Expr::Op { bin, lhs, rhs, .. } => write!(f, "({} {} {})", lhs, bin, rhs),
      Expr::HasAttr { lhs, path } => {
        write!(f, "(({}) ? ", lhs)?;
        show_attrpath(f, path)?;
        f.write_str(")")
      }
      Expr::Not(e) => write!(f, "(!{})", e),
    }
  }
}

impl Display for Attrs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, (key, val)) in self.attrs.iter().enumerate() {
      if i > 0 {
        f.write_str(" ")?;
      }
      if val.inherited {
        write!(f, "inherit {};", key)?;
      } else {
        write!(f, "{} = {};", key, val.rhs)?;
      }
    }
    Ok(())
  }
}

impl Display for Formals {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut first = true;
    write!(f, "{{ ")?;
    for item in &self.formals {
      if !first {
        write!(f, ", ")?;
      }
      first = false;
      write!(f, "{}", item.name)?;
      if let Some(e) = &item.def {
        write!(f, " ? {}", e)?;
      }
    }
    if self.ellipsis {
      if first {
        write!(f, "...")?
      } else {
        write!(f, ", ...")?
      }
    }
    if first {
      write!(f, "}}")
    } else {
      write!(f, " }}")
    }
  }
}

pub type StaticEnvRef = Writable<StaticEnv>;

#[derive(Default)]
pub struct StaticEnv {
  pub is_with: bool,
  pub up: Option<StaticEnvRef>,
  pub vars: HashMap<Ident, usize>,
}
