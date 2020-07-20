use crate::{
  bail,
  error::Result,
  primop, primop2, primop3,
  thunk::{StaticScope, ThunkId},
  value::Value,
  Eval,
};
use primop::Primop;
use syntax::expr::Ident;

pub mod attrs;
pub mod fetch;
pub mod functions;
pub mod json;
pub mod lists;
pub mod strings;
pub mod sys;
pub mod versions;

pub fn init_primops(eval: &mut Eval) {
  eval.toplevel.insert(
    "import".into(),
    eval.new_value(Primop::single("import", sys::import)),
  );
  eval.toplevel.insert(
    "abort".into(),
    eval.new_value(Primop::single("abort", nix_abort)),
  );
  eval.toplevel.insert(
    "toString".into(),
    eval.new_value(Primop::single("toString", strings::coerce_to_string)),
  );
  let nixver = eval.new_value(Value::string_bare("2.3.7"));
  eval
    .toplevel
    .insert("__nixPath".into(), eval.new_value(sys::mk_nix_path(&eval)));
  eval.toplevel.insert(
    "map".into(),
    eval.new_value(primop2!("map", lists::map_list)),
  );
  eval
    .toplevel
    .insert("null".into(), eval.new_value(Value::Null));
  eval
    .toplevel
    .insert("true".into(), eval.new_value(Value::Bool(true)));
  eval
    .toplevel
    .insert("false".into(), eval.new_value(Value::Bool(false)));
  eval.toplevel.insert(
    "removeAttrs".into(),
    eval.new_value(primop2!("removeAttrs", attrs::remove_attrs)),
  );
  eval.toplevel.insert(
    "builtins".into(),
    eval.new_value(Value::AttrSet({
      let mut builtins = StaticScope::new();
      builtins.insert("nixVersion".into(), nixver);
      builtins.insert(
        "addErrorContext".into(),
        eval.new_value(primop2!("addErrorContext", add_error_context)),
      );
      builtins.insert(
        "attrNames".into(),
        eval.new_value(Primop::single("attrNames", attrs::attr_names)),
      );
      builtins.insert(
        "concatLists".into(),
        eval.new_value(Primop::single("concatLists", lists::concat_lists)),
      );
      builtins.insert(
        "compareVersions".into(),
        eval.new_value(primop2!("compareVersions", versions::compare_versions)),
      );
      builtins.insert(
        "concatStringsSep".into(),
        eval.new_value(primop2!("concatStringsSep", strings::concat_strings_sep)),
      );
      builtins.insert(
        "currentSystem".into(),
        eval.new_value(Value::string_bare("x86_64-linux")),
      );
      builtins.insert("elem".into(), eval.new_value(primop2!("elem", lists::elem)));
      builtins.insert(
        "elemAt".into(),
        eval.new_value(primop2!("elemAt", lists::elem_at)),
      );
      builtins.insert(
        "fromJSON".into(),
        eval.new_value(Primop::single("fromJSON", json::from_json)),
      );
      builtins.insert(
        "functionArgs".into(),
        eval.new_value(Primop::single("functionArgs", functions::function_args)),
      );
      builtins.insert(
        "genericClosure".into(),
        eval.new_value(Primop::single("genericClosure", attrs::generic_closure)),
      );
      builtins.insert(
        "getEnv".into(),
        eval.new_value(Primop::single("getEnv", sys::get_env)),
      );
      builtins.insert(
        "genList".into(),
        eval.new_value(primop2!("genList", lists::gen_list)),
      );
      builtins.insert(
        "head".into(),
        eval.new_value(Primop::single("head", lists::head)),
      );
      builtins.insert(
        "filter".into(),
        eval.new_value(primop2!("filter", lists::filter)),
      );
      builtins.insert(
        "lessThan".into(),
        eval.new_value(primop2!("lessThan", prim_less_than)),
      );
      builtins.insert(
        "tail".into(),
        eval.new_value(Primop::single("tail", lists::tail)),
      );
      builtins.insert(
        "isString".into(),
        eval.new_value(Primop::single("isString", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::String { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isAttrs".into(),
        eval.new_value(Primop::single("isAttrs", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::AttrSet { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isBool".into(),
        eval.new_value(Primop::single("isBool", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::Bool { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isFunction".into(),
        eval.new_value(Primop::single("isFunction", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::Primop { .. } => true,
            Value::Lambda { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isList".into(),
        eval.new_value(Primop::single("isList", |e, i| {
          Ok(Value::Bool(match e.value_of(i)? {
            Value::List(_) => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "hasAttr".into(),
        eval.new_value(primop2!("hasAttr", attrs::has_attr)),
      );
      builtins.insert(
        "intersectAttrs".into(),
        eval.new_value(primop2!("intersectAttrs", attrs::intersect_attrs)),
      );
      builtins.insert(
        "listToAttrs".into(),
        eval.new_value(Primop::single("listToAttrs", attrs::list_to_attrs)),
      );
      builtins.insert(
        "pathExists".into(),
        eval.new_value(Primop::single("pathExists", sys::path_exists)),
      );
      builtins.insert(
        "readFile".into(),
        eval.new_value(Primop::single("readFile", sys::read_file)),
      );
      builtins.insert(
        "removeAttrs".into(),
        eval.new_value(primop2!("removeAttrs", attrs::remove_attrs)),
      );
      builtins.insert(
        "length".into(),
        eval.new_value(Primop::single("length", |e, i| {
          Ok(Value::Int(e.value_list_of(i)?.len() as _))
        })),
      );
      builtins.insert(
        "stringLength".into(),
        eval.new_value(Primop::single("stringLength", |e, i| {
          Ok(Value::Int(e.value_str_of(i)?.0.len() as _))
        })),
      );
      builtins.insert(
        "substring".into(),
        eval.new_value(primop3!("substring", strings::substring)),
      );
      builtins.insert(
        "toJSON".into(),
        eval.new_value(Primop::single("toJSON", json::to_json_primop)),
      );
      builtins.insert(
        "tryEval".into(),
        eval.new_value(Primop::single("tryEval", try_eval)),
      );
      builtins
    })),
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
    attrs.insert(Ident::from("success"), eval.new_value(Value::Bool(false)));
    attrs.insert(Ident::from("value"), eval.new_value(Value::Bool(false)));
    Ok(Value::AttrSet(attrs))
  } else {
    attrs.insert(Ident::from("success"), eval.new_value(Value::Bool(true)));
    attrs.insert(Ident::from("value"), thing);
    Ok(Value::AttrSet(attrs))
  }
}

fn prim_less_than(eval: &Eval, lhs: ThunkId, rhs: ThunkId) -> Result<Value> {
  let lhs = eval.value_of(lhs)?;
  let rhs = eval.value_of(rhs)?;
  Ok(Value::Bool(crate::operators::less_than(lhs, rhs)?))
}
