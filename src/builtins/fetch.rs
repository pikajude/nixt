use crate::{
  bail,
  error::Result,
  thunk::{StaticScope, ThunkId},
  value::Value,
  Eval,
};
use syntax::expr::Ident;

pub fn fetch_tarball(eval: &Eval, args: ThunkId) -> Result<Value> {
  match eval.value_of(args)? {
    Value::AttrSet(a) => Ok(Value::AttrSet({
      let mut attrs = StaticScope::new();
      attrs.insert(
        Ident::from("outPath"),
        eval.new_value(Value::String {
          string: "/nix/store/foobar123".into(),
          context: Default::default(),
        }),
      );
      attrs
    })),
    Value::String { .. } => bail!("fetchTarball string"),
    _ => bail!("fetchTarball expects a URL or an attrset of arguments"),
  }
}
