use crate::{
  scope::StaticScope,
  todo,
  value::{Primop, Value, ValueRef},
  Eval, Result,
};
use std::path::PathBuf;
use syntax::span::{FileSpan, Spanned};

fn import(eval: &mut Eval, rhs: ValueRef) -> Result<Value> {
  let path: PathBuf = eval.force_path_like(rhs)?.node.into();
  assert!(path.is_absolute());
  Ok(Value::Pointer(futures::executor::block_on(
    eval.load(&path),
  )?))
}

pub fn find_file(eval: &mut Eval, search_path: ValueRef, filename: ValueRef) -> Result<PathBuf> {
  todo!()
}

pub fn load_inherent_scope(eval: &mut Eval, span: FileSpan) -> Result<ValueRef> {
  let builtins_id = eval.allocator.insert(Spanned::new(span, Value::Blackhole));
  let mut inherent_scope = StaticScope::new();
  let mut builtins = StaticScope::new();

  macro_rules! register {
    ($builtin_name:literal, $top_name:literal, $val:expr) => {{
      let __item = eval.allocator.insert(Spanned::new(span, $val));
      inherent_scope.insert($top_name.into(), __item);
      builtins.insert($builtin_name.into(), __item);
    }};
    ($name:literal, $val:expr) => {
      register!($name, $name, $val);
    };
  }

  register!("import", Value::Primop(Primop::static_("import", import)));
  register!(
    "findFile",
    "__findFile",
    Value::Primop(Primop::primop2("findFile", move |e, v1, v2| {
      Ok(Value::Path(find_file(e, v1, v2)?))
    }))
  );

  builtins.insert(
    "builtins".into(),
    eval
      .allocator
      .insert(Spanned::new(span, Value::Pointer(builtins_id))),
  );
  inherent_scope.insert(
    "builtins".into(),
    eval
      .allocator
      .insert(Spanned::new(span, Value::Attrset(builtins))),
  );

  *eval.allocator[builtins_id] = Value::Attrset(inherent_scope);
  Ok(builtins_id)
}
