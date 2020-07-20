use crate::{
  bail,
  error::Result,
  primop,
  primop::Primop,
  primop2, primop_inline,
  thunk::{StaticScope, Thunk, ThunkId},
  value::Value,
  Eval,
};
use async_std::path::{Path, PathBuf};
use futures::TryStreamExt;
use primop::Op;
use syntax::expr::Ident;

mod attrs;
mod lists;
mod sys;
mod versions;

pub fn init_primops(eval: &mut Eval) {
  eval
    .toplevel
    .insert("import".into(), eval.items.alloc(primop!("import", import)));
  eval.toplevel.insert(
    "abort".into(),
    eval.items.alloc(primop!("abort", nix_abort)),
  );
  eval.toplevel.insert(
    "toString".into(),
    eval.items.alloc(primop!("toString", coerce_to_string)),
  );
  let nixver = eval
    .items
    .alloc(Thunk::complete(Value::string_bare("2.3.7")));
  eval.toplevel.insert(
    "__nixPath".into(),
    eval.items.alloc(Thunk::complete(mk_nix_path(&eval))),
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
    "builtins".into(),
    eval.items.alloc(Thunk::complete(Value::AttrSet({
      let mut builtins = StaticScope::new();
      builtins.insert("nixVersion".into(), nixver);
      builtins.insert(
        "attrNames".into(),
        eval.items.alloc(primop!("attrNames", attrs::attr_names)),
      );
      builtins.insert(
        "concatLists".into(),
        eval
          .items
          .alloc(primop!("concatLists", lists::concat_lists)),
      );
      builtins.insert(
        "compareVersions".into(),
        eval
          .items
          .alloc(primop2!("compareVersions", versions::compare_versions)),
      );
      builtins.insert(
        "elemAt".into(),
        eval.items.alloc(primop2!("elemAt", lists::elem_at)),
      );
      builtins.insert(
        "getEnv".into(),
        eval.items.alloc(primop!("getEnv", sys::get_env)),
      );
      builtins.insert(
        "genList".into(),
        eval.items.alloc(primop2!("genList", lists::gen_list)),
      );
      builtins.insert(
        "isString".into(),
        eval.items.alloc(primop_inline!("isString", |e, i| {
          Ok(Value::Bool(match e.value_of(i).await? {
            Value::String { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isAttrs".into(),
        eval.items.alloc(primop_inline!("isAttrs", |e, i| {
          Ok(Value::Bool(match e.value_of(i).await? {
            Value::AttrSet { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isFunction".into(),
        eval.items.alloc(primop_inline!("isFunction", |e, i| {
          Ok(Value::Bool(match e.value_of(i).await? {
            Value::Primop { .. } => true,
            Value::Lambda { .. } => true,
            _ => false,
          }))
        })),
      );
      builtins.insert(
        "isList".into(),
        eval.items.alloc(Thunk::complete(Value::Primop(Primop {
          name: "isList",
          op: Op::Async(Box::new(move |e, i| {
            Box::pin(async move {
              Ok(Value::Bool(match e.value_of(i).await? {
                Value::List { .. } => true,
                _ => false,
              }))
            })
          })),
        }))),
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
          .alloc(primop!("listToAttrs", attrs::list_to_attrs)),
      );
      builtins.insert(
        "pathExists".into(),
        eval.items.alloc(primop!("pathExists", sys::path_exists)),
      );
      builtins.insert(
        "removeAttrs".into(),
        eval
          .items
          .alloc(primop2!("removeAttrs", attrs::remove_attrs)),
      );
      builtins.insert(
        "length".into(),
        eval.items.alloc(primop_inline!("length", |e, i| {
          Ok(Value::Int(e.value_list_of(i).await?.len() as _))
        })),
      );
      builtins.insert(
        "stringLength".into(),
        eval.items.alloc(primop_inline!("stringLength", |e, i| {
          Ok(Value::Int(e.value_str_of(i).await?.0.len() as _))
        })),
      );
      builtins
    }))),
  );
}

pub async fn import(eval: &Eval, path: ThunkId) -> Result<Value> {
  let path = eval.value_path_of(path).await?;
  let meta = path.metadata().await?;
  let r = if meta.is_dir() {
    eval.load_file(path.join("default.nix")).await?
  } else {
    eval.load_file(path).await?
  };
  Ok(Value::Ref(r))
}

pub async fn find_file(eval: &Eval, path: ThunkId, filename: &str) -> Result<PathBuf> {
  let entries = eval.value_list_of(path).await?;
  let target = Path::new(filename);
  let mut path_parts = target.components();
  let search_key = path_parts
    .next()
    .expect("must have at least one item in path")
    .as_os_str();
  let has_children = path_parts.as_path().iter().next().is_some();
  let add_children = move |p: PathBuf| {
    if has_children {
      p.join(path_parts.as_path())
    } else {
      p
    }
  };
  for entry in entries {
    let kv = eval.value_attrs_of(*entry).await?;
    let ((path, _), (prefix, _)) = futures::future::try_join(
      eval.value_str_of(*kv.get(&Ident::from("path")).unwrap()),
      eval.value_str_of(*kv.get(&Ident::from("prefix")).unwrap()),
    )
    .await?;
    if search_key == &*prefix {
      let full = add_children(path.to_string().into());
      if full.exists().await {
        return Ok(full);
      }
    } else if prefix.is_empty() {
      if let Ok(mut iter) = async_std::fs::read_dir(&*path).await {
        while let Some(next_item) = iter.try_next().await? {
          if next_item.file_name() == search_key {
            return Ok(add_children(next_item.path()));
          }
        }
      }
    }
  }
  bail!(
    "Entry `{}' was not found in the Nix search path",
    target.display()
  )
}

pub async fn nix_abort(eval: &Eval, msg: ThunkId) -> Result<Value> {
  let (msg, _) = eval.value_str_of(msg).await?;
  bail!("evaluation aborted with the message: {}", msg)
}

pub async fn coerce_to_string(eval: &Eval, obj: ThunkId) -> Result<Value> {
  let v = eval.value_of(obj).await?;
  Ok(match v {
    Value::Path(p) => Value::String {
      string: p.display().to_string(),
      context: Default::default(),
    },
    _ => bail!("not handled, coercing to string: {}", v.typename()),
  })
}

pub fn mk_nix_path(eval: &Eval) -> Value {
  let mut entries = vec![];
  for entry in get_nix_path() {
    let mut parts = entry.splitn(2, '=');
    let first = parts.next().unwrap();
    let second = parts.next();
    let (prefix, path) = match second {
      Some(x) => (first, x),
      None => ("", first),
    };
    let mut entry_attr = StaticScope::new();
    entry_attr.insert(
      "path".into(),
      eval.items.alloc(Thunk::complete(Value::String {
        string: path.into(),
        context: Default::default(),
      })),
    );
    entry_attr.insert(
      "prefix".into(),
      eval.items.alloc(Thunk::complete(Value::String {
        string: prefix.into(),
        context: Default::default(),
      })),
    );
    entries.push(
      eval
        .items
        .alloc(Thunk::complete(Value::AttrSet(entry_attr))),
    );
  }
  Value::List(entries)
}

fn is_uri(s: &str) -> bool {
  [
    "http://",
    "https://",
    "file://",
    "channel:",
    "channel://",
    "git://",
    "s3://",
    "ssh://",
  ]
  .iter()
  .any(|x| s.starts_with(x))
}

fn parse_nix_path(n: &str) -> Vec<&str> {
  let mut strings = vec![];
  let mut start = 0;
  let mut prev_colon = 0;
  for (next_colon, _) in n.match_indices(':') {
    if let Some(x) = n[prev_colon..next_colon].rfind('=') {
      if is_uri(&n[prev_colon + x + 1..]) {
        prev_colon = next_colon;
        continue;
      }
    }
    strings.push(&n[start..next_colon]);
    start = next_colon + 1;
  }
  if start < n.len() {
    strings.push(&n[start..]);
  }
  strings
}

fn get_nix_path() -> Vec<String> {
  if let Ok(n) = std::env::var("NIX_PATH") {
    parse_nix_path(&n)
      .into_iter()
      .map(|x| x.to_string())
      .collect()
  } else {
    vec![]
  }
}

#[macro_export]
macro_rules! primop3 {
  ($x:ident, $s:literal, $f:ident) => {
    pub async fn $x(_: &Eval, arg: ThunkId) -> Result<Value> {
      Ok(Value::Primop(Primop {
        name: concat!($s, "-app").into(),
        op: Box::new(move |eval, arg2| Box::pin($f(eval, arg, arg2))),
      }))
    }
  };
}

#[test]
fn test_nix_path() {
  assert_eq!(
    parse_nix_path(
      "nixpkgs=https://github.com:foo=bar:nixpkgs2=https://github.com:foo=bar:qux=:baz"
    ),
    &[
      "nixpkgs=https://github.com",
      "foo=bar",
      "nixpkgs2=https://github.com",
      "foo=bar",
      "qux=",
      "baz"
    ]
  )
}
