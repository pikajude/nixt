use crate::arena::Arena;
use codespan::FileId;

pub mod expr;
pub mod lexer;
pub mod parse;
pub mod span;

pub fn parse<'input>(
  file_id: FileId,
  arena: &Arena<expr::Expr>,
  input: &'input str,
) -> Result<expr::ExprRef, parse::ParseError> {
  let ast = parse::ExprParser::new().parse(arena, file_id, lexer::Lexer::new(input));
  ast.map_err(|e| parse::ParseError {
    id: file_id,
    perror: e
      .map_token(|t| t.to_string())
      .map_error(|(a, b, c)| (a, b.to_string(), c)),
  })
}
