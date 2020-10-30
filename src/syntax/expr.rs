use super::span::Spanned;
use crate::arena::Id;
use string_cache::DefaultAtom;

pub mod symbol;

pub type Ident = DefaultAtom;

/// This addresses an `Expr` in the allocation table. Span information is kept
/// alongside the id for convenience.
pub type ExprRef = Spanned<ExprId>;
pub type ExprId = Id<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
  Pos,
  Int(i64),
  Float(f64),
  Var(Ident),
  Str(Str),
  IndStr(IndStr),
  Path(Path),
  Uri(String),
  Lambda(Lambda),
  Assert(Assert),
  With(With),
  Let(Let),
  List(List),
  If(If),
  Unary(Unary),
  Binary(Binary),
  Member(Member),
  Apply(Apply),
  Select(Select),
  AttrSet(AttrSet),
}

impl Expr {
  pub fn unary(a: Spanned<UnaryOp>, b: ExprRef) -> Self {
    Self::Unary(Unary { op: a, operand: b })
  }

  pub fn is_plain_string(&self) -> bool {
    match self {
      Self::Str(Str { body, .. }) | Self::IndStr(IndStr { body, .. }) => {
        body.len() == 1 && matches!(&body[0], StrPart::Plain(_))
      }
      _ => false,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Str {
  pub open: Spanned<symbol::Quote>,
  pub body: Vec<StrPart>,
  pub close: Spanned<symbol::Quote>,
}

#[derive(Debug, Clone)]
pub struct IndStr {
  pub open: Spanned<symbol::SpecialQuote>,
  pub body: Vec<StrPart>,
  pub close: Spanned<symbol::SpecialQuote>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
  pub argument: Spanned<LambdaArg>,
  pub colon: Spanned<symbol::Colon>,
  pub body: ExprRef,
}

#[derive(Debug, Clone)]
pub struct Assert {
  pub assert: Spanned<symbol::Assert>,
  pub cond: ExprRef,
  pub semi: Spanned<symbol::Semi>,
  pub expr: ExprRef,
}

#[derive(Debug, Clone)]
pub struct With {
  pub with: Spanned<symbol::With>,
  pub env: ExprRef,
  pub semi: Spanned<symbol::Semi>,
  pub expr: ExprRef,
}

#[derive(Debug, Clone)]
pub struct Let {
  pub let_: Spanned<symbol::Let>,
  pub binds: Spanned<Vec<Spanned<Binding>>>,
  pub in_: Spanned<symbol::In>,
  pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct List {
  pub open: Spanned<symbol::BracketL>,
  pub elems: Vec<ExprRef>,
  pub close: Spanned<symbol::BracketR>,
}

#[derive(Debug, Clone)]
pub struct If {
  pub if_: Spanned<symbol::If>,
  pub cond: ExprRef,
  pub then: Spanned<symbol::Then>,
  pub rhs1: ExprRef,
  pub else_: Spanned<symbol::Else>,
  pub rhs2: ExprRef,
}

#[derive(Debug, Clone, Copy)]
pub struct Unary {
  pub op: Spanned<UnaryOp>,
  pub operand: ExprRef,
}

#[derive(Debug, Clone, Copy)]
pub struct Binary {
  pub lhs: ExprRef,
  pub op: Spanned<BinaryOp>,
  pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct Member {
  pub lhs: ExprRef,
  pub question: Spanned<symbol::Question>,
  pub path: AttrPath,
}

#[derive(Debug, Clone, Copy)]
pub struct Apply {
  pub lhs: ExprRef,
  pub rhs: ExprRef,
}

#[derive(Debug, Clone)]
pub struct Select {
  pub lhs: ExprRef,
  pub dot: Spanned<symbol::Dot>,
  pub path: AttrPath,
  pub or: Option<SelectOr>,
}

#[derive(Debug, Clone, Copy)]
pub struct SelectOr {
  pub or: Spanned<symbol::Or>,
  pub fallback: ExprRef,
}

#[derive(Debug, Clone)]
pub struct AttrSet {
  pub rec: Option<Spanned<symbol::Rec>>,
  pub open: Spanned<symbol::BraceL>,
  pub attrs: Vec<Spanned<Binding>>,
  pub close: Spanned<symbol::BraceR>,
}

#[derive(Debug, Clone)]
pub enum Path {
  Plain(String),
  Home(String),
  Nix {
    open: Spanned<symbol::AngleL>,
    path: String,
    close: Spanned<symbol::AngleR>,
  },
}

#[derive(Debug, Clone)]
pub enum StrPart {
  Plain(String),
  Quote {
    open: Spanned<symbol::Curly>,
    quote: ExprRef,
    close: Spanned<symbol::BraceR>,
  },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Not,
  Negate,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  Concat,
  Impl,
  Or,
  And,
  Eq,
  Neq,
  Le,
  Leq,
  Ge,
  Geq,
  Update,
  Add,
  Sub,
  Mul,
  Div,
}

impl BinaryOp {
  pub fn as_str(self) -> &'static str {
    use BinaryOp::*;
    match self {
      Concat => "++",
      Impl => "->",
      Or => "||",
      And => "&&",
      Eq => "==",
      Neq => "!=",
      Le => "<",
      Leq => "<=",
      Ge => ">",
      Geq => ">=",
      Update => "//",
      Add => "+",
      Sub => "-",
      Mul => "*",
      Div => "/",
    }
  }
}

impl UnaryOp {
  pub fn as_str(self) -> &'static str {
    match self {
      UnaryOp::Not => "!",
      UnaryOp::Negate => "-",
    }
  }
}

#[derive(Debug, Clone)]
pub enum Binding {
  Plain {
    path: AttrPath,
    eq: Spanned<symbol::Eq>,
    rhs: ExprRef,
    semi: Spanned<symbol::Semi>,
  },
  Inherit {
    inherit: Spanned<symbol::Inherit>,
    from: Option<InheritFrom>,
    attrs: AttrList,
    semi: Spanned<symbol::Semi>,
  },
}

#[derive(Debug, Clone)]
pub struct InheritFrom {
  pub open: Spanned<symbol::ParenL>,
  pub from: ExprRef,
  pub close: Spanned<symbol::ParenR>,
}

#[derive(Debug, Clone)]
pub struct AttrList(pub Vec<Spanned<AttrName>>); // inherit foo bar baz;
#[derive(Debug, Clone)]
pub struct AttrPath(pub Vec<Spanned<AttrName>>); // foo.bar.baz

#[derive(Debug, Clone)]
pub enum AttrName {
  Plain(Ident),
  Str {
    open: Spanned<symbol::Quote>,
    body: Vec<StrPart>,
    close: Spanned<symbol::Quote>,
  },
  Dynamic {
    open: Spanned<symbol::Curly>,
    quote: ExprRef,
    close: Spanned<symbol::BraceR>,
  },
}

#[derive(Debug, Clone)]
pub enum LambdaArg {
  Plain(Ident),
  Formals(Formals),
}

#[derive(Debug, Clone)]
pub struct Formals {
  pub open: Spanned<symbol::BraceL>,
  pub args: Vec<Spanned<Formal>>,
  pub ellipsis: Option<Spanned<symbol::Ellipsis>>,
  pub at: Option<FormalsAt>,
  pub close: Spanned<symbol::BraceR>,
}

#[derive(Debug, Clone)]
pub struct FormalsAt {
  pub at: Spanned<symbol::At>,
  pub name: Spanned<Ident>,
}

#[derive(Debug, Clone)]
pub struct Formal {
  pub arg_name: Spanned<Ident>,
  pub fallback: Option<FormalDef>,
}

#[derive(Debug, Clone)]
pub struct FormalDef {
  pub question: Spanned<symbol::Question>,
  pub default: ExprRef,
}
