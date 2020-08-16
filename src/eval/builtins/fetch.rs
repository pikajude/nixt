use crate::{
  eval::{thunk::ThunkId, value::Value, Eval},
  prelude::*,
  syntax::expr::Ident,
  util::*,
};
use std::borrow::Cow;

pub fn fetch_tarball(eval: &Eval, args: ThunkId) -> Result<Value> {
  fetch(eval, args, "fetchTarball", true, "source")
}

fn fetch(eval: &Eval, args: ThunkId, who: &'static str, unpack: bool, name: &str) -> Result<Value> {
  let mut name: Option<&str> = Some(name);
  let mut hash: Option<Hash> = None;

  let url = match eval.value_of(args)? {
    Value::AttrSet(a) => {
      let mut a = a.clone();
      let murl = match a.remove(&Ident::from("url")) {
        Some(x) => Some(eval.value_string_of(x)?),
        None => None,
      };
      hash = match a.remove(&Ident::from("sha256")) {
        Some(x) => Some(Hash::new_allow_empty(
          eval.value_string_of(x)?,
          Some(HashType::SHA256),
        )?),
        None => None,
      };
      name = match a.remove(&Ident::from("name")) {
        Some(v) => Some(eval.value_string_of(v)?),
        None => None,
      };
      if let Some(k) = a.keys().next() {
        bail!("unsupported argument to {}: `{}'", who, k);
      }
      murl.ok_or_else(|| anyhow!("`url' argument is required"))?
    }
    Value::String { string, context } => {
      if context.is_empty() {
        string
      } else {
        bail!("expected a string without context")
      }
    }
    _ => bail!("builtins.{} requires a URL or attrset of arguments", who),
  };

  let name = name.unwrap_or_else(|| {
    Path::new(url)
      .file_name()
      .expect("a URL should have a filepath in it")
      .to_str()
      .expect("oh")
  });

  let url = pretend_resolve(url);

  let out_path = if unpack {
    let output = async_std::task::block_on(crate::fetch::download_tarball(
      eval.store.as_ref(),
      &url,
      name,
      hash.is_some(),
    ))?;
    eval.store.print_store_path(&output)
  } else {
    let output = crate::fetch::download_file(eval.store.as_ref(), &url, name, hash.is_some())?;
    eval.store.print_store_path(&output.store_path)
  };

  Ok(Value::String {
    string: out_path.clone(),
    context: std::iter::once(out_path).collect(),
  })
}

fn pretend_resolve(s: &str) -> Cow<str> {
  if let Some(rest) = s.strip_prefix("channel:") {
    Cow::Owned(format!(
      "https://nixos.org/channels/{}/nixexprs.tar.xz",
      rest
    ))
  } else {
    Cow::Borrowed(s)
  }
}
