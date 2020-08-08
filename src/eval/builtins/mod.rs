use super::AssertFailure;
use crate::{
  eval::{context::StaticScope, primop::Primop, thunk::ThunkId, value::Value, Eval},
  primop, primop2, primop3, primop_inline,
  syntax::expr::Ident,
  util::*,
};

pub mod attrs;
pub mod derivation;
pub mod fetch;
pub mod functions;
pub mod json;
pub mod lists;
pub mod strings;
pub mod sys;
pub mod versions;

pub fn init_primops(eval: &mut Eval) -> Result<()> {
  eval.toplevel.insert(
    "import".into(),
    eval.new_value(Primop::single("import", sys::import)),
  );
  let corepkg = eval.load_file(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/corepkgs/derivation.nix"
  ))?;
  eval.toplevel.insert("derivation".into(), corepkg);
  eval
    .toplevel
    .insert("abort".into(), eval.new_value(primop!("abort", nix_abort)));
  eval.toplevel.insert(
    "toString".into(),
    eval.new_value(primop!("toString", strings::prim_to_string)),
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
    "baseNameOf".into(),
    eval.new_value(primop!("baseNameOf", sys::base_name_of)),
  );
  eval.toplevel.insert(
    "placeholder".into(),
    eval.new_value(primop!("placeholder", strings::placeholder)),
  );
  eval.toplevel.insert(
    "removeAttrs".into(),
    eval.new_value(primop2!("removeAttrs", attrs::remove_attrs)),
  );
  eval.toplevel.insert(
    "derivationStrict".into(),
    eval.new_value(primop!("derivationStrict", derivation::derivation_strict)),
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
        eval.new_value(primop!("attrNames", attrs::attr_names)),
      );
      builtins.insert(
        "concatLists".into(),
        eval.new_value(primop!("concatLists", lists::concat_lists)),
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
        "foldl'".into(),
        eval.new_value(primop3!("foldl'", lists::foldl_strict)),
      );
      builtins.insert(
        "map".into(),
        eval.new_value(primop2!("map", lists::map_list)),
      );
      builtins.insert(
        "elemAt".into(),
        eval.new_value(primop2!("elemAt", lists::elem_at)),
      );
      builtins.insert(
        "fetchTarball".into(),
        eval.new_value(primop!("fetchTarball", fetch::fetch_tarball)),
      );
      builtins.insert(
        "fromJSON".into(),
        eval.new_value(primop!("fromJSON", json::from_json)),
      );
      builtins.insert(
        "functionArgs".into(),
        eval.new_value(primop!("functionArgs", functions::function_args)),
      );
      builtins.insert(
        "genericClosure".into(),
        eval.new_value(primop!("genericClosure", attrs::generic_closure)),
      );
      builtins.insert(
        "getAttr".into(),
        eval.new_value(primop2!("getAttr", attrs::get_attr)),
      );
      builtins.insert(
        "getEnv".into(),
        eval.new_value(primop!("getEnv", sys::get_env)),
      );
      builtins.insert(
        "genList".into(),
        eval.new_value(primop2!("genList", lists::gen_list)),
      );
      builtins.insert("head".into(), eval.new_value(primop!("head", lists::head)));
      builtins.insert(
        "filter".into(),
        eval.new_value(primop2!("filter", lists::filter)),
      );
      builtins.insert(
        "lessThan".into(),
        eval.new_value(primop2!("lessThan", prim_less_than)),
      );
      builtins.insert("tail".into(), eval.new_value(primop!("tail", lists::tail)));
      builtins.insert(
        "isString".into(),
        eval.new_value(primop_inline!("isString", |e, i| {
          Ok(Value::Bool(matches!(e.value_of(i)?, Value::String { .. })))
        })),
      );
      builtins.insert(
        "isAttrs".into(),
        eval.new_value(primop_inline!("isAttrs", |e, i| {
          Ok(Value::Bool(matches!(e.value_of(i)?, Value::AttrSet { .. })))
        })),
      );
      builtins.insert(
        "isBool".into(),
        eval.new_value(primop_inline!("isBool", |e, i| {
          Ok(Value::Bool(matches!(e.value_of(i)?, Value::Bool(_))))
        })),
      );
      builtins.insert(
        "isFunction".into(),
        eval.new_value(primop_inline!("isFunction", |e, i| {
          Ok(Value::Bool(
            matches!(e.value_of(i)?, Value::Primop{..} | Value::Lambda{..}),
          ))
        })),
      );
      builtins.insert(
        "isList".into(),
        eval.new_value(primop_inline!("isList", |e, i| {
          Ok(Value::Bool(matches!(e.value_of(i)?, Value::List(_))))
        })),
      );
      builtins.insert(
        "isNull".into(),
        eval.new_value(primop_inline!("isNull", |e, i| {
          Ok(Value::Bool(matches!(e.value_of(i)?, Value::Null)))
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
        eval.new_value(primop!("listToAttrs", attrs::list_to_attrs)),
      );
      builtins.insert(
        "match".into(),
        eval.new_value(primop2!("match", strings::matches)),
      );
      builtins.insert(
        "storeDir".into(),
        eval.new_value(Value::string_bare(
          eval.store.store_path().to_string_lossy(),
        )),
      );
      builtins.insert(
        "split".into(),
        eval.new_value(primop2!("split", strings::split)),
      );
      builtins.insert(
        "pathExists".into(),
        eval.new_value(primop!("pathExists", sys::path_exists)),
      );
      builtins.insert(
        "parseDrvName".into(),
        eval.new_value(primop!("parseDrvName", strings::parse_drv_name)),
      );
      builtins.insert(
        "readFile".into(),
        eval.new_value(primop!("readFile", sys::read_file)),
      );
      builtins.insert(
        "removeAttrs".into(),
        eval.new_value(primop2!("removeAttrs", attrs::remove_attrs)),
      );
      builtins.insert(
        "replaceStrings".into(),
        eval.new_value(primop3!("replaceStrings", strings::replace_strings)),
      );
      builtins.insert(
        "length".into(),
        eval.new_value(primop_inline!("length", |e, i| {
          Ok(Value::Int(e.value_list_of(i)?.len() as _))
        })),
      );
      builtins.insert(
        "stringLength".into(),
        eval.new_value(primop_inline!("stringLength", |e, i| {
          Ok(Value::Int(e.value_with_context_of(i)?.0.len() as _))
        })),
      );
      builtins.insert(
        "substring".into(),
        eval.new_value(primop3!("substring", strings::substring)),
      );
      builtins.insert(
        "toJSON".into(),
        eval.new_value(primop!("toJSON", json::to_json_primop)),
      );
      builtins.insert(
        "tryEval".into(),
        eval.new_value(primop!("tryEval", try_eval)),
      );
      builtins.insert(
        "unsafeGetAttrPos".into(),
        eval.new_value(primop2!("unsafeGetAttrPos", attrs::unsafe_get_attr_pos)),
      );
      builtins.insert(
        "unsafeDiscardStringContext".into(),
        eval.new_value(primop!(
          "unsafeDiscardStringContext",
          strings::discard_context
        )),
      );
      builtins
    })),
  );
  Ok(())
}

fn nix_abort(eval: &Eval, msg: ThunkId) -> Result<Value> {
  let (msg, _) = eval.value_with_context_of(msg)?;
  bail!("evaluation aborted with the message: {}", msg)
}

fn add_error_context(eval: &Eval, ctx: ThunkId, value: ThunkId) -> Result<Value> {
  if let Err(mut e) = eval.value_of(value) {
    let (ctx, _) = eval.value_with_context_of(ctx)?;
    e = e.context(ctx.to_string());
    Err(e)
  } else {
    Ok(Value::Ref(value))
  }
}

fn try_eval(eval: &Eval, thing: ThunkId) -> Result<Value> {
  let mut attrs = StaticScope::new();
  if let Err(e) = eval.value_of(thing) {
    if let Some(AssertFailure { .. }) = e.downcast_ref() {
      attrs.insert(Ident::from("success"), eval.new_value(Value::Bool(false)));
      attrs.insert(Ident::from("value"), eval.new_value(Value::Bool(false)));
      Ok(Value::AttrSet(attrs))
    } else {
      Err(e)
    }
  } else {
    attrs.insert(Ident::from("success"), eval.new_value(Value::Bool(true)));
    attrs.insert(Ident::from("value"), thing);
    Ok(Value::AttrSet(attrs))
  }
}

fn prim_less_than(eval: &Eval, lhs: ThunkId, rhs: ThunkId) -> Result<Value> {
  let lhs = eval.value_of(lhs)?;
  let rhs = eval.value_of(rhs)?;
  Ok(Value::Bool(crate::eval::operators::less_than(lhs, rhs)?))
}
