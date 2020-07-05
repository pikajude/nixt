use crate::{
  error::ErrorKind,
  scope::StaticScope,
  todo,
  value::{Primop, Value, ValueRef},
  Eval, Result,
};
use std::path::{Path, PathBuf};
use syntax::{
  expr::Ident,
  span::{FileSpan, Spanned},
};
use tokio::fs;

async fn import(eval: &Eval, rhs: ValueRef) -> Result<Value> {
  let path = eval.force_path_like(rhs).await?.node;
  let data = tokio::fs::metadata(path).await?;
  let fut = if data.is_dir() {
    eval.load(&path.join("default.nix")).await?
  } else {
    eval.load(path).await?
  };
  Ok(Value::Pointer(fut))
}

pub async fn find_file(eval: &Eval, search_path: ValueRef, filename: ValueRef) -> Result<PathBuf> {
  let entries = eval
    .force_list(search_path)
    .await?
    .iter()
    .copied()
    .collect::<Vec<_>>();
  let to_find = Path::new(eval.force_str(filename).await?.node);
  let mut path_parts = to_find.components();
  let search_key = path_parts
    .next()
    .expect("must have at least one item in path entry")
    .as_os_str();
  let has_children = path_parts.as_path().iter().next().is_some();
  let add_children = move |p: PathBuf| {
    if has_children {
      p.join(path_parts.as_path())
    } else {
      p
    }
  };
  for r in entries {
    let attrset = eval.force_attrs(r).await?;
    let path = eval
      .force_str(*attrset.get(&Ident::from("path")).unwrap())
      .await?
      .node;
    let prefix = eval
      .force_str(*attrset.get(&Ident::from("prefix")).unwrap())
      .await?
      .node;
    if search_key == prefix {
      let full = add_children(path.into());
      if fs::metadata(&full).await.is_ok() {
        return Ok(full);
      }
    } else if prefix.is_empty() {
      if let Ok(mut iter) = fs::read_dir(path).await {
        while let Ok(Some(next_item)) = iter.next_entry().await {
          if next_item.file_name() == search_key {
            return Ok(add_children(next_item.path()));
          }
        }
      }
    }
  }
  Err(
    ErrorKind::User(format!(
      "entry `{}' was not found in the Nix search path",
      to_find.display()
    ))
    .into(),
  )
}

pub fn load_inherent_scope(eval: &mut Eval, span: FileSpan) -> Result<ValueRef> {
  let builtins_id = eval
    .allocator
    .insert(Spanned::new(span, Value::blackhole()));
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

  register!(
    "import",
    Value::Primop(Primop::static_("import", move |e, v| Box::pin(import(
      e, v
    ))))
  );
  register!(
    "findFile",
    "__findFile",
    Value::Primop(Primop::primop2("findFile", move |e, v1, v2| {
      Box::pin(async move { Ok(Value::Path(find_file(e, v1, v2).await?)) })
    }))
  );

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
      eval.allocator.insert(Spanned::new(
        span,
        Value::String {
          s: path.into(),
          context: Default::default(),
        },
      )),
    );
    entry_attr.insert(
      "prefix".into(),
      eval.allocator.insert(Spanned::new(
        span,
        Value::String {
          s: prefix.into(),
          context: Default::default(),
        },
      )),
    );

    entries.push(
      eval
        .allocator
        .insert(Spanned::new(span, Value::Attrset(entry_attr))),
    );
  }

  register!("nixPath", "__nixPath", Value::List(entries));

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

  eval
    .allocator
    .swap_deref(builtins_id, Value::Attrset(inherent_scope));
  Ok(builtins_id)
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
