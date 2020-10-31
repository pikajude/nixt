use crate::{
  archive::{DumpResult, PathFilter},
  path_info::ValidPathInfo,
  prelude::*,
  store::{CheckSigsFlag, FileIngestionMethod, RepairFlag},
};
use curl::easy::{Easy, HttpVersion};
use indicatif::ProgressBar;
use std::sync::Once;

mod cache;
mod tar;

#[derive(Debug)]
pub struct DownloadFile {
  pub store_path: StorePath,
  pub etag: Option<String>,
  pub effective_url: String,
}

pub fn download_file<S: Store>(
  store: &S,
  url: &str,
  name: &str,
  _immutable: bool,
) -> Result<DownloadFile> {
  info!("fetching {}", url);
  let mut download_file = tempfile::NamedTempFile::new()?;
  let response = ureq::get(url).call();
  let etag = response.header("etag").map(|x| x.to_string());
  let effective_url = response.get_url().to_string();

  let response_size = std::io::copy(&mut response.into_reader(), &mut download_file)?;

  // two tempfiles needed because we use the first one to determine the content
  // length, since servers don't always provide one.
  let mut destfile = tempfile::NamedTempFile::new()?;

  let DumpResult {
    contents_hash,
    nar_hash,
    nar_size,
  } = crate::archive::dump_contents_hash(
    response_size as _,
    &mut download_file.reopen()?,
    &mut destfile,
    HashType::SHA256,
  )?;

  let fixed_output_path = store.make_fixed_output_path(
    FileIngestionMethod::Flat,
    &contents_hash,
    name,
    &mut std::iter::empty(),
    false,
  )?;
  let mut path_info = ValidPathInfo::new(fixed_output_path.clone(), nar_hash);
  path_info.nar_size = Some(nar_size as u64);

  store.add_to_store_from_source(
    path_info,
    &mut destfile.reopen()?,
    RepairFlag::NoRepair,
    CheckSigsFlag::NoCheckSigs,
  )?;

  Ok(DownloadFile {
    store_path: fixed_output_path,
    etag,
    effective_url,
  })
}

pub fn download_tarball<S: Store>(
  store: &S,
  url: &str,
  name: &str,
  immutable: bool,
) -> Result<StorePath> {
  let cache_key = maplit::hashmap! {
    "type" => "tarball",
    "url" => url,
    "name" => name
  };

  let cached = cache::CACHE.lookup_expired(store, &cache_key)?;

  if let Some(c) = cached {
    if !c.expired {
      return Ok(c.path);
    }
  }

  let unpack_to = tempfile::tempdir()?;

  let file = download_file(store, url, name, immutable)?;

  tar::Archive::open(store.to_real_path(&file.store_path)?)?.extract_to(&unpack_to)?;

  let mut dir_reader = std::fs::read_dir(&unpack_to)?;

  let first_item = dir_reader
    .next()
    .ok_or_else(|| anyhow!("tarball `{}' appears to be empty", url))??;

  if dir_reader.next().is_some() {
    bail!("tarball `{}' contains more than 1 top-level file", url);
  }

  let new_path = store.add_to_store_from_path(
    name,
    &first_item.path(),
    FileIngestionMethod::Recursive,
    HashType::SHA256,
    &PathFilter::none(),
    RepairFlag::NoRepair,
  )?;

  let info =
    serde_json::json!({ "lastModified": first_item.metadata()?.modified()?, "etag": file.etag });

  cache::CACHE.add(cache_key, info, &new_path, immutable)?;

  Ok(new_path)
}

pub fn fetchurl(derivation: &Derivation, progress: &ProgressBar) -> Result<Hash> {
  let store_path = derivation.get_env("out")?;
  let main_url = derivation.get_env("url")?;
  let unpack = derivation.env.get("unpack").map_or(false, |x| x == "1");
  let expected_hash = derivation
    .env
    .get("sha256")
    .map(|s| Hash::decode_with_type(s, HashType::SHA256, false))
    .transpose()?;

  let mut easy = Easy::new();
  easy.url(main_url)?;
  easy.follow_location(true)?;
  easy.max_redirections(10)?;
  easy.useragent("curl/Nix/1.0.0")?;
  easy.http_version(HttpVersion::V11)?;
  easy.progress(true)?;

  crossbeam::thread::scope(|s| {
    let mut xfer = make_pipe(s, move |writer| {
      let mut transfer = easy.transfer();
      transfer.write_function(|data| {
        writer.write_all(data).unwrap();
        Ok(data.len())
      })?;
      transfer.progress_function(|total_down, partial_down, _, _| {
        if total_down > 0.0 {
          progress.set_length(total_down as u64);
        }
        progress.set_position(partial_down as u64);
        true
      })?;
      transfer.header_function(|f| {
        trace!(
          "curl header: {}",
          std::str::from_utf8(f).unwrap_or("<bad UTF-8>")
        );
        true
      })?;
      transfer.perform()?;
      Ok::<_, curl::Error>(())
    });

    if unpack {
      crate::archive::restore_path(store_path, xfer)?;
    } else {
      let mut f = fs::File::create(store_path)?;
      std::io::copy(&mut xfer, &mut f)?;
    }

    if derivation.env.get("executable").map_or(false, |x| x == "1") {
      fs::set_permissions(store_path, fs::Permissions::from_mode(0o755))?;
    }

    debug!("computing hash of fetched file(s)");
    let actual_hash = if unpack {
      panic!("TODO: hash an archive")
    } else {
      Hash::hash_file(store_path, HashType::SHA256)?.0
    };

    if let Some(e) = expected_hash {
      if actual_hash != e {
        bail!(
          "hash mismatch in file downloaded from {}:\n  wanted {:?}, got {:?}",
          main_url,
          e.encode(Encoding::Base32),
          actual_hash.encode(Encoding::Base32)
        );
      }
    }

    Ok(actual_hash)
  })
  .unwrap()
}

static PRELOAD_NSS: Once = Once::new();

pub fn preload_nss() {
  PRELOAD_NSS.call_once(|| {
    if dns_lookup::getaddrinfo(
      Some("invalid.domain.name.used.for.resolvers"),
      Some("http"),
      None,
    )
    .is_ok()
    {
      panic!("huh? this should have failed")
    }
  })
}
