use crate::{
  archive::{DumpResult, PathFilter},
  path_info::ValidPathInfo,
  prelude::*,
  store::{CheckSigsFlag, FileIngestionMethod, RepairFlag},
};

mod cache;
mod tar;

#[derive(Debug)]
pub struct DownloadFile {
  pub store_path: StorePath,
  pub etag: Option<String>,
  pub effective_url: String,
}

pub fn download_file(
  store: &dyn Store,
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
    &path_info,
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

pub fn download_tarball(
  store: &dyn Store,
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

  tar::TarArchive::open(store.to_real_path(&file.store_path)?)?.extract_to(&unpack_to)?;

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
