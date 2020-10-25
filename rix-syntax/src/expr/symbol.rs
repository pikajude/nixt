macro_rules! symbol {
  ($($i:ident),+) => {
    $(
    #[derive(Clone, Copy, Debug, Default)]
    pub struct $i;
    )+
  };
}

symbol!(
  BraceL,
  BraceR,
  BracketL,
  BracketR,
  ParenL,
  ParenR,
  Rec,
  Dot,
  Or,
  Question,
  If,
  Then,
  Else,
  In,
  Semi,
  Colon,
  Curly,
  Eq,
  Inherit,
  AngleL,
  AngleR,
  Let,
  With,
  Assert,
  At,
  Ellipsis,
  Quote,
  SpecialQuote
);
