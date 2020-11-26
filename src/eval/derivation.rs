use super::Eval;
use crate::{
  prelude::*,
  value::{Value, ValueRef},
};

pub fn prim_derivation_strict(eval: &Eval, pos: Pos, args: Vec<ValueRef>) -> Result<Value> {
  let mut attrs = eval.force_attrs(&args[0], pos)?.clone();
  attrs.insert(
    ident!("outPath"),
    Located {
      pos,
      v: writable(Value::string_bare("/no-such-path")),
    },
  );
  attrs.insert(
    ident!("drvPath"),
    Located {
      pos,
      v: writable(Value::string_bare("/no-such-path.drv")),
    },
  );
  attrs.insert(
    ident!("type"),
    Located {
      pos,
      v: writable(Value::string_bare("derivation")),
    },
  );
  Ok(Value::Attrs(readable(attrs)))
}
