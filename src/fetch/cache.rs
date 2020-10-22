use crate::prelude::*;
use parking_lot::Mutex;
use rusqlite::{params, Connection};
use serde::ser::Serialize;
use std::{
  collections::HashMap,
  fmt::Debug,
  time::{Duration, SystemTime},
};

static SCHEMA: &str = "create table if not exists Cache (
    input     text not null,
    info      text not null,
    path      text not null,
    immutable integer not null,
    timestamp integer not null,
    primary key (input)
);";

pub struct Cache(Mutex<Connection>);

lazy_static! {
  pub static ref CACHE: Cache = {
    let db_file = dirs_next::cache_dir()
      .expect("no XDG dirs found")
      .join("nix")
      .join("fetcher-cache-v1.sqlite");
    std::fs::create_dir_all(db_file.parent().unwrap()).expect("Unable to create cache directory.");
    let conn = Connection::open(db_file).expect("unable to open cache file");
    conn.execute_batch(SCHEMA).expect("unable to set up cache");
    Cache(Mutex::new(conn))
  };
}

impl Cache {
  pub fn add(
    &self,
    _key: impl Serialize,
    _info: impl Serialize,
    _store_path: &StorePath,
    _immutable: bool,
  ) -> Result<()> {
    Ok(())
  }

  pub fn lookup_expired<S: Serialize + Debug, N: Store + ?Sized>(
    &self,
    store: &N,
    data: S,
  ) -> Result<Option<CacheResult>> {
    if let Some(r) = self
      .0
      .lock()
      .query_row_and_then(
        "select info, path, immutable, timestamp from Cache where input = ?",
        params![serde_json::to_string(&data)?],
        |row| {
          Ok(CacheResultTmp {
            info_json: row.get("info")?,
            path: store.parse_store_path(Path::new(&row.get::<_, String>("path")?))?,
            immutable: row.get("immutable")?,
            timestamp: row.get("timestamp")?,
          })
        },
      )
      .optional()?
    {
      store.add_temp_root(&r.path)?;
      if !store.is_valid_path(&r.path)? {
        debug!("ignoring disappeared cache entry `{:?}'", &data);
        return Ok(None);
      }
      debug!(
        "using cache entry `{:?}' -> '{}', '{}'",
        &data,
        &r.info_json,
        store.print_store_path(&r.path)
      );
      Ok(Some(CacheResult {
        info: serde_json::from_str(&r.info_json)?,
        expired: !r.immutable
          && (Settings::get().tarball_ttl.is_zero()
            || to_time(r.timestamp) + Settings::get().tarball_ttl < SystemTime::now()),
        path: r.path,
      }))
    } else {
      Ok(None)
    }
  }
}

fn to_time(i: i64) -> SystemTime {
  SystemTime::UNIX_EPOCH + Duration::from_secs(i as u64)
}

struct CacheResultTmp {
  info_json: String,
  path: StorePath,
  immutable: bool,
  timestamp: i64,
}

pub struct CacheResult {
  pub info: HashMap<String, String>,
  pub path: StorePath,
  pub expired: bool,
}
