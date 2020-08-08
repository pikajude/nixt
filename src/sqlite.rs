use crate::prelude::*;
use rusqlite::{Connection, DatabaseName};

#[derive(Deref, DerefMut)]
pub struct Sqlite(Connection);

impl Sqlite {
  pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
    let mut conn = Connection::open(path)?;
    if log_enabled!(log::Level::Trace) {
      conn.trace(Some(|x| trace!("{}", x)));
    }
    Ok(Self(conn))
  }

  pub fn set_is_cache(&self) -> Result<()> {
    self.pragma_update(None, "synchronous", &"off")?;
    self.pragma_update(Some(DatabaseName::Main), "journal_mode", &"truncate")?;
    Ok(())
  }
}
