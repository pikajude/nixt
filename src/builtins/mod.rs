use crate::{
  bail,
  error::Result,
  primop, primop2, primop3,
  thunk::{StaticScope, Thunk, ThunkId},
  value::Value,
  Eval,
};
use primop::Primop;
use syntax::expr::Ident;

pub mod attrs;
pub mod functions;
pub mod json;
pub mod lists;
pub mod strings;
pub mod sys;
pub mod versions;

pub fn init_primops(eval: &mut Eval) {
  eval.toplevel.insert(
    "import".into(),
    eval.items.alloc(Primop::single("import", sys::import)),
  );
  eval.toplevel.insert(
    "abort".into(),
    eval.items.alloc(Primop::single("abort", nix_abort)),
  );
  eval.toplevel.insert(
    "toString".into(),
    eval
      .items
      .alloc(Primop::single("toString", strings::coerce_to_string)),
  );
  let nixver = eval
    .items
    .alloc(Thunk::complete(Value::string_bare("2.3.7")));
  eval.toplevel.insert(
    "__nixPath".into(),
    eval.items.alloc(Thunk::complete(sys::mk_nix_path(&eval))),
  );
  eval.toplevel.insert(
    "map".into(),
    eval.items.alloc(primop2!("map", lists::map_list)),
  );
  eval.toplevel.insert(
    "null".into(),
    eval.items.alloc(Thunk::complete(Value::Null)),
  );
  eval.toplevel.insert(
    "true".into(),
    eval.items.alloc(Thunk::complete(Value::Bool(true))),
  );
  eval.toplevel.insert(
    "false".into(),
    eval.items.alloc(Thunk::complete(Value::Bool(false))),
  );
  eval.toplevel.insert(
    "removeAttrs".into(),
    eval
      .items
      .alloc(primop2!("removeAttrs", attrs::remove_attrs)),
  );
  eval.toplevel.insert(
    "builtins".into(),
    eval.items.alloc(Thunk::complete(Value::AttrSet({
      let mut builtins = StaticScope::new();
      builtins.insert("nixVersion".into(), nixver);
      builtins.insert(
        "addErrorContext".into(),
        eval
          .items
          .alloc(primop2!("addErrorContext", add_error_context)),
      );
      builtins.insert(
        "attrNames".into(),
        eval
          .items
          .alloc(Primop::single("attrNames", attrs::attr_names)),
      );
      builtins.insert(
        "concatLists".into(),
        eval
          .items
          .alloc(Primop::single("concatLists", lists::concat_lists)),
      );
      builtins.insert(
        "compareVersions".into(),
        eval
          .items
          .alloc(primop2!("compareVersions", versions::compare_versions)),
      );
      builtins.insert(
        "concatStringsSep".into(),
        eval
          .items
          .alloc(primop2!("concatStringsSep", strings::concat_strings_sep)),
      );
      builtins.insert(
        "currentSystem".into(),
        eval
          .items
          .alloc(Thunk::complete(Value::string_bare("x86_64-linux"))),
      );
      builtins.insert(
        "elem".into(),
        eval.items.alloc(primop2!("elem", lists::elem)),
      );
      builtins.insert(
        "elemAt".into(),
        eval.items.alloc(primop2!("elemAt", lists::elem_at)),
      );
      builtins.insert(
        "functionArgs".into(),
        eval
          .items
          .alloc(Primop::single("functionArgs", functions::function_args)),
      );
      builtins.insert(
        "genericClosure".into(),
        eval
          .items
          .alloc(Primop::single("genericClosure", attrs::generic_closure)),
      );
      builtins.insert(
        "getEnv".into(),
        eval.items.alloc(Primop::single("getEnv", sys::get_env)),
      );
      builtins.insert(
        "genList".into(),
        eval.items.alloc(primop2!("genList", lists::gen_list)),
      );
      builtins.insert(
        "head".into(),
        eval.items.alloc(Primop::single("head", lists::head)),
      );
      builtins.insert(
        "filter".into(),
        eval.items.alloc(primop2!("filter", lists::filter)),
      );
      builtins.insert(
        "tail".into(),
        eval.items.alloc(Primop::single("tail", lists::tail)),
      );
      builtins.insert(
        "isString".into(),
        eval.items.alloc(Primop::single("isString", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::String { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isAttrs".into(),
        eval.items.alloc(Primop::single("isAttrs", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::AttrSet { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isBool".into(),
        eval.items.alloc(Primop::single("isBool", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::Bool { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isFunction".into(),
        eval.items.alloc(Primop::single("isFunction", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::Primop { .. } => true,
            Value::Lambda { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isList".into(),
        eval.items.alloc(Primop::single("isList", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::List(_) => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "intersectAttrs".into(),
        eval
          .items
          .alloc(primop2!("intersectAttrs", attrs::intersect_attrs)),
      );
      builtins.insert(
        "listToAttrs".into(),
        eval
          .items
          .alloc(Primop::single("listToAttrs", attrs::list_to_attrs)),
      );
      builtins.insert(
        "pathExists".into(),
        eval
          .items
          .alloc(Primop::single("pathExists", sys::path_exists)),
      );
      builtins.insert(
        "removeAttrs".into(),
        eval
          .items
          .alloc(primop2!("removeAttrs", attrs::remove_attrs)),
      );
      builtins.insert(
        "length".into(),
        eval.items.alloc(Primop::single("length", |e, i| {
          Ok(Value::Int(e.value_list_of(i)?.len() as _))
        })),
      );
      builtins.insert(
        "stringLength".into(),
        eval.items.alloc(Primop::single("stringLength", |e, i| {
          Ok(Value::Int(e.value_str_of(i)?.0.len() as _))
        })),
      );
      builtins.insert(
        "substring".into(),
        eval.items.alloc(primop3!("substring", strings::substring)),
      );
      builtins.insert(
        "toJSON".into(),
        eval
          .items
          .alloc(Primop::single("toJSON", json::to_json_primop)),
      );
      builtins.insert(
        "tryEval".into(),
        eval.items.alloc(Primop::single("tryEval", try_eval)),
      );
      builtins
    }))),
  );
}

fn nix_abort(eval: &Eval, msg: ThunkId) -> Result<Value> {
  let (msg, _) = eval.value_str_of(msg)?;
  bail!("evaluation aborted with the message: {}", msg)
}

fn add_error_context(eval: &Eval, ctx: ThunkId, value: ThunkId) -> Result<Value> {
  if let Err(mut e) = eval.value_of(value) {
    let (ctx, _) = eval.value_str_of(ctx)?;
    e.err = e.err.context(ctx.to_string());
    Err(e)
  } else {
    Ok(Value::Ref(value))
  }
}

fn try_eval(eval: &Eval, thing: ThunkId) -> Result<Value> {
  let mut attrs = StaticScope::new();
  if let Err(e) = eval.value_of(thing) {
    warn!("tryEval is currently wrong");
    warn!("we are eating: {:?}", e);
    attrs.insert(
      Ident::from("success"),
      eval.items.alloc(Thunk::complete(Value::Bool(false))),
    );
    attrs.insert(
      Ident::from("value"),
      eval.items.alloc(Thunk::complete(Value::Bool(false))),
    );
    Ok(Value::AttrSet(attrs))
  } else {
    attrs.insert(
      Ident::from("success"),
      eval.items.alloc(Thunk::complete(Value::Bool(true))),
    );
    attrs.insert(Ident::from("value"), thing);
    Ok(Value::AttrSet(attrs))
  }
}
