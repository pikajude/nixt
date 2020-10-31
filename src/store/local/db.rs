use crate::{path_info::ValidPathInfo, prelude::*, sqlite::Sqlite};
use rusqlite::{named_params, DatabaseName};
use std::{
  collections::BTreeSet,
  time::{Duration, SystemTime},
};

static QUERY_PATH_INFO: &str = "select id, hash, registrationTime, deriver, narSize, ultimate, \
                                sigs, ca from ValidPaths where path = :path";

static QUERY_REFERENCES: &str =
  "select path from Refs join ValidPaths on reference = id where referrer = :id";

static INSERT_REFERENCE: &str =
  "insert or replace into Refs (referrer, reference) values (:referrer, :reference)";

static GET_PATH_ID: &str = "select id from ValidPaths where path = :path";

static REGISTER_VALID_PATHS: &str =
  "insert or replace into ValidPaths (path, hash, registrationTime, deriver, narSize, ultimate, \
   sigs, ca) values (:path, :hash, :registrationTime, :deriver, :narSize, :ultimate, :sigs, :ca)";

pub fn init(db: &Sqlite, create: bool) -> Result<()> {
  db.busy_timeout(Duration::from_millis(60 * 60 * 1000))?;
  db.pragma_update(None, "foreign_keys", &1u8)?;
  db.pragma_update(None, "synchronous", &"normal")?;
  let cur_mode = db.pragma_query_value(Some(DatabaseName::Main), "journal_mode", |r| {
    r.get::<_, String>(0)
  })?;
  let new_mode = "wal";
  if cur_mode != new_mode {
    db.pragma_update(Some(DatabaseName::Main), "journal_mode", &new_mode)?;
  }
  if new_mode == "wal" {
    db.pragma_update(None, "wal_autocheckpoint", &40000i64)?;
  }
  if create {
    db.execute_batch(include_str!("schema.sql"))?;
  }
  Ok(())
}

pub fn insert_valid_paths<S: Store + ?Sized>(
  db: &mut Sqlite,
  store: &S,
  paths: Vec<ValidPathInfo>,
) -> Result<()> {
  let txn = db.transaction()?;

  for path_info in &paths {
    txn.execute_named(
      REGISTER_VALID_PATHS,
      named_params! {
        ":path": store.print_store_path(&path_info.store_path),
        ":hash": path_info.nar_hash.encode_with_type(Encoding::Base16),
        ":registrationTime": path_info.registration_time.duration_since(SystemTime::UNIX_EPOCH)?.as_secs() as i64,
        ":deriver": path_info.deriver.as_ref().map(|r|store.print_store_path(r)),
        ":narSize": path_info.nar_size.unwrap_or(0) as i64,
        ":ultimate": path_info.ultimate,
        ":sigs": itertools::join(&path_info.signatures, " "),
        ":ca": ""
      },
    )?;
  }
  txn.commit()?;

  // XXX: these are done in two separate steps because the outputs may depend on
  // each other, but are not necessarily topologically sorted
  let txn = db.transaction()?;
  for path_info in paths.iter().filter(|x| !x.references.is_empty()) {
    let from_id = txn.query_row_named(
      GET_PATH_ID,
      named_params! {":path": store.print_store_path(path_info.store_path())},
      |row| row.get::<_, i64>(0),
    )?;

    for r in &path_info.references {
      let to_id = txn.query_row_named(
        GET_PATH_ID,
        named_params! {":path": store.print_store_path(r)},
        |row| row.get::<_, i64>(0),
      )?;
      txn.execute_named(
        INSERT_REFERENCE,
        named_params! {
          ":referrer": from_id,
          ":reference": to_id
        },
      )?;
    }
  }

  txn.commit()?;
  Ok(())
}

pub fn get_path_info<S: Store + ?Sized>(
  db: &Sqlite,
  store: &S,
  path: &StorePath,
) -> Result<Option<ValidPathInfo>> {
  let mut stmt0 = db.prepare(QUERY_PATH_INFO)?;

  let mut maybe_valid = stmt0.query_and_then_named(
    named_params! { ":path": store.print_store_path(path).as_str() },
    |row| -> Result<ValidPathInfo> {
      let maybe_deriver = row.get::<_, Option<String>>("deriver")?;
      Ok(ValidPathInfo {
        id: row.get::<_, i64>("id")?,
        store_path: path.clone(),
        deriver: maybe_deriver
          .map(|x| store.parse_store_path(x))
          .transpose()?,
        nar_hash: Hash::decode(&row.get::<_, String>("hash")?)?,
        references: Default::default(),
        registration_time: SystemTime::UNIX_EPOCH
          + Duration::from_secs(row.get::<_, i64>("registrationTime")?.try_into()?),
        nar_size: Some(row.get::<_, i64>("narSize")?.try_into()?),
        signatures: Default::default(),
        content_addressed: row.get("ca")?,
        ultimate: row.get("ultimate")?,
      })
    },
  )?;

  if let Some(mut pinfo) = maybe_valid.next().transpose()? {
    pinfo.references = get_references(db, store, pinfo.id)?;
    // gather references
    Ok(Some(pinfo))
  } else {
    Ok(None)
  }
}

pub fn get_references<S: Store + ?Sized>(
  db: &Sqlite,
  store: &S,
  id: i64,
) -> Result<BTreeSet<StorePath>> {
  let mut stmt0 = db.prepare(QUERY_REFERENCES)?;

  let items = stmt0
    .query_and_then_named(named_params! { ":id": id }, |row| {
      store.parse_store_path(row.get::<_, String>("path")?)
    })?
    .collect();
  items
}
