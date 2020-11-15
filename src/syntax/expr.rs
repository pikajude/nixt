use super::{
  parse::{Located, Pos},
  UserError,
};
use crate::prelude::*;
use std::{collections::HashMap, fmt, fmt::Display, path::PathBuf};

pub type ExprRef = Writable<Expr>;

#[derive(Debug, EnumAsInner)]
pub enum Expr {
  Pos {
    pos: Pos,
  },
  Var(Var),
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
  HasAttr {
    lhs: ExprRef,
    path: AttrPath,
  },
  Not(ExprRef),
  Negate(ExprRef),
}

impl Expr {
  pub fn bin(operator: Bin, args: Located<(Self, Self)>) -> Self {
    Self::Op {
      bin: operator,
      pos: args.pos,
      lhs: writable(args.v.0),
      rhs: writable(args.v.1),
    }
  }
}

fn show_attrpath(f: &mut fmt::Formatter, path: &[AttrName]) -> fmt::Result {
  for (i, attr) in path.iter().enumerate() {
    if i > 0 {
      f.write_str(".")?;
    }
    match attr {
      AttrName::Static(k) => k.fmt(f)?,
      AttrName::Dynamic(e) => write!(f, "\"${{{}}}\"", e.read())?,
    }
  }
  Ok(())
}

#[derive(Debug, Display)]
pub enum Bin {
  #[display(fmt = "")]
  Apply,
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
  #[display(fmt = "<")]
  Le,
  #[display(fmt = "<=")]
  Leq,
  #[display(fmt = ">")]
  Ge,
  #[display(fmt = ">=")]
  Geq,
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

#[derive(Debug)]
pub enum AttrName {
  Static(Ident),
  Dynamic(ExprRef),
}

#[derive(Debug)]
pub struct Var {
  pub pos: Pos,
  pub name: Ident,
  pub from_with: bool,
  pub level: usize,
  pub displ: usize,
}

impl Var {
  pub fn new(pos: Pos, name: Ident) -> Self {
    Self {
      pos,
      name,
      from_with: false,
      level: 0,
      displ: 0,
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
  pos: Pos,
  name: Ident,
  arg: LambdaArg,
  body: ExprRef,
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

  fn add_attr(&mut self, pos: Pos, path: &[AttrName], rhs: Expr) -> Result<(), Located<UserError>> {
    match path {
      [] => panic!("invariant violation: empty attrpath produced by parser"),
      [AttrName::Static(i)] => {
        if let Some(x) = self.attrs.get(i) {
          if let (Expr::Attrs(ae), Some(j)) = (rhs, x.rhs.write().as_attrs_mut()) {
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
              rhs: writable(rhs),
              displ: 0,
            },
          );
        }
      }
      [AttrName::Dynamic(e)] => self.dyn_attrs.push(DynAttrDef {
        pos,
        name: e.clone(),
        value: writable(rhs),
      }),
      [AttrName::Static(i), rest @ ..] => {
        if let Some(j) = self.attrs.get(i) {
          if !j.inherited {
            return j
              .rhs
              .write()
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
              rhs: writable(Expr::Attrs(new_attrs)),
              displ: 0,
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
          value: writable(Expr::Attrs(new_attrs)),
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
  pub displ: usize,
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
      Expr::Var(n) => n.name.fmt(f),
      Expr::Int { n } => n.fmt(f),
      Expr::Float { f: g } => g.fmt(f),
      Expr::String { s } => write!(f, "{:?}", s),
      Expr::Path { path } => path.display().fmt(f),
      Expr::Select { lhs, def, path, .. } => {
        write!(f, "({}).", lhs.read())?;
        show_attrpath(f, path)?;
        if let Some(x) = def {
          write!(f, " or ({})", x.read())?;
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
        write!(f, ": {}", body.read())?;
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
      Expr::Assert { cond, body, .. } => write!(f, "assert {}; {}", cond.read(), body.read()),
      Expr::With { env, body, .. } => write!(f, "(with {}; {})", env.read(), body.read()),
      Expr::Let { attrs, body } => write!(f, "(let {} in {})", attrs, body.read()),
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
      } => write!(
        f,
        "(if {} then {} else {})",
        cond.read(),
        rhs1.read(),
        rhs2.read()
      ),
      Expr::Op {
        bin: Bin::Apply,
        lhs,
        rhs,
        ..
      } => write!(f, "({} {})", lhs.read(), rhs.read()),
      Expr::Op { bin, lhs, rhs, .. } => write!(f, "({} {} {})", lhs.read(), bin, rhs.read()),
      Expr::HasAttr { lhs, path } => {
        write!(f, "(({}) ? ", lhs.read())?;
        show_attrpath(f, path)?;
        f.write_str(")")
      }
      Expr::Not(e) => write!(f, "(!{})", e.read()),
      Expr::Negate(e) => write!(f, "(-{})", e.read()),
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
        write!(f, "{} = {};", key, val.rhs.read())?;
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
        write!(f, " ? {}", e.read())?;
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
