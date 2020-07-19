use derive_more::Display;
use lazy_static::lazy_static;
use regex::Regex;
use std::num::{ParseFloatError, ParseIntError};

#[derive(Clone, Debug, Display)]
pub enum Token<'a> {
  If,
  Then,
  Else,
  Assert,
  With,
  Let,
  In,
  Rec,
  Inherit,
  OrKw,
  Ellipsis,

  Eq,
  Neq,
  Leq,
  Geq,
  And,
  Or,
  Impl,
  Update,
  Concat,

  Id(&'a str),
  Int(i64),
  Float(f64),

  DollarCurly,
  IndStringOpen,
  IndStringClose,

  Str(&'a str),
  IndStr(&'a str),
  Path(&'a str),
  HomePath(&'a str),
  NixPath(&'a str),
  Uri(&'a str),
  Any(char),
}

lazy_static! {
  pub static ref ID: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_'-]*").unwrap();
  static ref INT: Regex = Regex::new(r"^[0-9]+").unwrap();
  static ref FLOAT: Regex =
    Regex::new(r"^(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?").unwrap();
  static ref PATH: Regex = Regex::new(r"^[a-zA-Z0-9\._\-\+]*(/[a-zA-Z0-9\._\-\+]+)+/?").unwrap();
  static ref HPATH: Regex = Regex::new(r"^\~(/[a-zA-Z0-9\._\-\+]+)+/?").unwrap();
  static ref SPATH: Regex = Regex::new(r"^<[a-zA-Z0-9\._\-\+]+(/[a-zA-Z0-9\._\-\+]+)*>").unwrap();
  static ref URI: Regex =
    Regex::new(r"^[a-zA-Z][a-zA-Z0-9\+\-\.]*:[a-zA-Z0-9%/\?:@\&=\+\$,\-_\.!\~\*']+").unwrap();
  static ref OPER: Regex = Regex::new(r"^(\.\.\.|==|!=|<=|>=|&&|\|\||->|//|\+\+)").unwrap();
  static ref STRING_INNER_0: Regex =
    Regex::new(r#"^([^\$"\\]|\$[^\{"\\]|\\(?s).|\$\\(?s).)*\$/""#).unwrap();
  static ref STRING_INNER_1: Regex =
    Regex::new(r#"^([^\$"\\]|\$[^\{"\\]|\\(?s).|\$\\(?s).)+"#).unwrap();
  static ref STRING_END_INVALID: Regex = Regex::new(r"^(\$|\\|\$\\)").unwrap();
  static ref IND_STRING_START: Regex = Regex::new(r"^''( *\n)?").unwrap();
  static ref IND_STRING_INNER: Regex = Regex::new(r"^([^\$']|\$[^\{']|'[^'\$])+").unwrap();
  static ref IND_STRING_ANY: Regex = Regex::new(r"^''\\(?s).").unwrap();
  static ref WHITESPACE: Regex = Regex::new(r"^[ \t\r\n]+").unwrap();
  static ref COMMENT_SINGLE: Regex = Regex::new(r"^#[^\r\n]*").unwrap();
  static ref COMMENT_MULTI: Regex = Regex::new(r"^/\*([^*]|\*+[^*/])*\*+/").unwrap();
}

#[derive(Clone, Copy, Debug)]
enum Mode {
  Default,
  String,
  IndString,
}

#[derive(Debug, Display)]
pub enum LexError<'a> {
  InvalidFloat(ParseFloatError),
  InvalidInt(ParseIntError),
  #[display(fmt = "unrecognized input at {}", _0)]
  UnrecognizedInput(&'a str),
}

#[derive(Clone)]
pub struct Lexer<'a> {
  _input: &'a str,
  at: usize,
  mode: Vec<Mode>,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Self {
    Self {
      _input: input,
      at: 0,
      mode: vec![],
    }
  }

  fn input(&self) -> &'a str {
    &self._input[self.at..]
  }

  fn mode(&self) -> Mode {
    self.mode.last().copied().unwrap_or(Mode::Default)
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Result<(usize, Token<'a>, usize), (usize, LexError<'a>, usize)>;

  // complex due to macros, but they capture local variables so fuck it
  #[allow(clippy::cognitive_complexity)]
  fn next(&mut self) -> Option<Self::Item> {
    if self.input().is_empty() {
      return None;
    }

    let start = self.at;
    let input = self.input();

    macro_rules! token_ {
      ($regex:ident, $cb:expr) => {
        if let Some(m) = $regex.find(input) {
          debug_assert!(m.start() == 0);
          self.at += m.end();
          return Some($cb(m.as_str()).map(|n| (start, n, self.at)));
        }
      };
    }

    macro_rules! token {
      ($regex:ident, $cb:expr) => {
        if let Some(m) = $regex.find(input) {
          debug_assert!(m.start() == 0);
          self.at += m.end();
          return Some(Ok((start, $cb(m.as_str()), self.at)));
        }
      };
    };

    macro_rules! skip {
      ($regex:ident) => {
        if let Some(m) = $regex.find(input) {
          debug_assert!(m.start() == 0);
          self.at += m.end();
          return self.next();
        }
      };
    }

    macro_rules! lit {
      ($lit:literal, $cb:expr) => {
        if input.starts_with($lit) {
          self.at += $lit.len();
          return Some(Ok((start, $cb(), self.at)));
        }
      };
    }

    match self.mode() {
      Mode::Default => {
        token!(PATH, Token::Path);
        token!(HPATH, Token::HomePath);
        token!(SPATH, Token::NixPath);
        token!(URI, Token::Uri);

        token!(ID, |s| match s {
          "if" => Token::If,
          "then" => Token::Then,
          "else" => Token::Else,
          "assert" => Token::Assert,
          "with" => Token::With,
          "let" => Token::Let,
          "in" => Token::In,
          "rec" => Token::Rec,
          "inherit" => Token::Inherit,
          "or" => Token::OrKw,
          x => Token::Id(x),
        });

        token!(OPER, |s| match s {
          "..." => Token::Ellipsis,
          "==" => Token::Eq,
          "!=" => Token::Neq,
          "<=" => Token::Leq,
          ">=" => Token::Geq,
          "&&" => Token::And,
          "||" => Token::Or,
          "->" => Token::Impl,
          "//" => Token::Update,
          "++" => Token::Concat,
          _ => unreachable!(),
        });

        token_!(INT, |i: &str| Ok(Token::Int(match i.parse::<i64>() {
          Ok(i) => i,
          Err(i) => return Err((start, LexError::InvalidInt(i), self.at)),
        })));
        token_!(FLOAT, |i: &str| Ok(Token::Float(match i.parse::<f64>() {
          Ok(i) => i,
          Err(i) => return Err((start, LexError::InvalidFloat(i), self.at)),
        })));

        lit!("${", || {
          self.mode.push(Mode::Default);
          Token::DollarCurly
        });
        lit!("}", || {
          self.mode.pop();
          Token::Any('}')
        });
        lit!("{", || {
          self.mode.push(Mode::Default);
          Token::Any('{')
        });
        lit!("\"", || {
          self.mode.push(Mode::String);
          Token::Any('"')
        });

        token!(IND_STRING_START, |_: &str| {
          self.mode.push(Mode::IndString);
          Token::IndStringOpen
        });

        skip!(WHITESPACE);
        skip!(COMMENT_SINGLE);
        skip!(COMMENT_MULTI);

        let ch = input.chars().next().unwrap();
        self.at += ch.len_utf8();
        return Some(Ok((start, Token::Any(ch), self.at)));
      }
      Mode::String => {
        token!(STRING_INNER_0, Token::Str);
        token!(STRING_INNER_1, Token::Str);

        lit!("${", || {
          self.mode.push(Mode::Default);
          Token::DollarCurly
        });
        lit!("\"", || {
          self.mode.pop();
          Token::Any('"')
        });

        token!(STRING_END_INVALID, Token::Str);
      }
      Mode::IndString => {
        token!(IND_STRING_INNER, Token::IndStr);
        lit!("''$", || Token::IndStr("$"));
        lit!("${", || {
          self.mode.push(Mode::Default);
          Token::DollarCurly
        });
        lit!("$", || Token::IndStr("$"));
        lit!("'''", || Token::IndStr("''"));
        token!(IND_STRING_ANY, Token::IndStr);
        lit!("''", || {
          self.mode.pop();
          Token::IndStringClose
        });
        lit!("'", || Token::IndStr("'"));
      }
    }

    Some(Err((start, LexError::UnrecognizedInput(input), self.at)))
  }
}
