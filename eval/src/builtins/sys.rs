use crate::{
  builtins::strings::coerce_new_string,
  thunk::{StaticScope, ThunkId},
  value::Value,
  Eval,
};
use async_std::{
  fs,
  path::{Path, PathBuf},
};
use futures::TryStreamExt;
use nix_util::*;
use syntax::expr::Ident;

pub async fn get_env(eval: &Eval, varname: ThunkId) -> Result<Value> {
  let varname = eval.value_string_of(varname).await?;
  match std::env::var(String::from(varname)) {
    Ok(s) => Ok(Value::string_bare(s)),
    Err(_) => Ok(Value::string_bare("")),
  }
}

pub async fn path_exists(eval: &Eval, path: ThunkId) -> Result<Value> {
  Ok(Value::Bool(match eval.value_of(path).await? {
    Value::String { string, .. } => fs::metadata(string).await.is_ok(),
    Value::Path(p) => p.exists().await,
    _ => false,
  }))
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

pub async fn base_name_of(eval: &Eval, path: ThunkId) -> Result<Value> {
  let path = eval.value_path_of(path).await?;
  Ok(Value::string_bare(
    path
      .iter()
      .last()
      .context("baseNameOf called with an empty path")?
      .to_string_lossy(),
  ))
}

pub async fn read_file(eval: &Eval, path: ThunkId) -> Result<Value> {
  let path = eval.value_path_of(path).await?;
  Ok(Value::string_bare(fs::read_to_string(path).await?))
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
    let path = eval
      .value_string_of(*kv.get(&Ident::from("path")).unwrap())
      .await?;

    let (_context, prefix) =
      coerce_new_string(eval, *kv.get(&Ident::from("prefix")).unwrap(), false, false).await?;

    if !_context.is_empty() {
      warn!("try to realise context {:?}", _context);
    }

    if search_key == &*prefix {
      let full = add_children(path.to_string().into());
      if full.exists().await {
        return Ok(full);
      }
    } else if prefix.is_empty() {
      if let Ok(mut iter) = fs::read_dir(&*path).await {
        while let Some(next_item) = iter.try_next().await? {
          let next_item = next_item;
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

pub fn mk_nix_path(eval: &Eval) -> Value {
  let mut entries = vec![];
  for entry in get_nix_path().into_iter().chain(std::iter::once(
    "nix=/nix/store/qk8iz5x8qwp6m3580fpp31f6fgf51ial-nix-2.3.7/share/nix/corepkgs".to_string(),
  )) {
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
      eval.new_value(Value::String {
        string: path.into(),
        context: Default::default(),
      }),
    );
    entry_attr.insert(
      "prefix".into(),
      eval.new_value(Value::String {
        string: prefix.into(),
        context: Default::default(),
      }),
    );
    entries.push(eval.new_value(Value::AttrSet(entry_attr)));
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
