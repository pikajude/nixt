use crate::base32;
use nix_util::Result;
use std::{cmp::Ordering, convert::TryInto, fmt, path::PathBuf, str::FromStr};

const HASH_BYTES: usize = 20;
const HASH_CHARS: usize = 32;

#[derive(thiserror::Error, Debug)]
pub enum Error {
  #[error("path is not in the nix store: `{}'", _0.display())]
  NotInStore(PathBuf),
  #[error("invalid filepath for store: `{}'", _0.display())]
  InvalidFilepath(PathBuf),
  #[error("invalid store path name: {0:?}")]
  InvalidStorePathName(String),
}

#[derive(Clone, PartialEq, Eq, std::hash::Hash, Debug, Display)]
#[display(fmt = "{}-{}", hash, name)]
pub struct Path {
  hash: Hash,
  name: Name,
}

impl PartialOrd for Path {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.to_string().partial_cmp(&other.to_string())
  }
}

impl Ord for Path {
  fn cmp(&self, other: &Self) -> Ordering {
    self.to_string().cmp(&other.to_string())
  }
}

lazy_static! {
  pub static ref DUMMY: Path = Path {
    hash: Hash([0xffu8; HASH_BYTES]),
    name: Name(String::from("x")),
  };
}

impl Path {
  pub fn from_parts(bytes: &[u8], name: &str) -> Result<Self> {
    Ok(Self {
      hash: Hash(
        bytes
          .try_into()
          .map_err(|_| crate::hash::Error::WrongHashLen(bytes.len()))?,
      ),
      name: name.parse()?,
    })
  }
}

#[derive(Clone, PartialEq, Eq, std::hash::Hash, Debug, Deref)]
pub struct Hash([u8; HASH_BYTES]);

impl fmt::Display for Hash {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut buf = vec![0; HASH_CHARS];
    base32::encode_into(&self.0, &mut buf);
    f.write_str(unsafe { std::str::from_utf8_unchecked(&buf) })
  }
}

impl Ord for Hash {
  fn cmp(&self, other: &Self) -> Ordering {
    // Historically we've sorted store paths by their base32
    // serialization, but our base32 encodes bytes in reverse
    // order. So compare them in reverse order as well.
    self.0.iter().rev().cmp(other.0.iter().rev())
  }
}

impl PartialOrd for Hash {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, std::hash::Hash, Debug, Deref, Display)]
#[deref(forward)]
pub struct Name(String);

impl FromStr for Name {
  type Err = Error;

  fn from_str(s: &str) -> Result<Self, Error> {
    fn is_valid_char(c: char) -> bool {
      c.is_ascii_alphabetic()
        || c.is_ascii_digit()
        || c == '+'
        || c == '-'
        || c == '.'
        || c == '_'
        || c == '?'
        || c == '='
    }

    if s.is_empty() {
      return Err(Error::InvalidStorePathName("".into()));
    }
    if s.len() > 211 {
      return Err(Error::InvalidStorePathName(s.into()));
    }
    if s.starts_with('.') || !s.chars().all(is_valid_char) {
      return Err(Error::InvalidStorePathName(s.into()));
    }
    Ok(Self(s.into()))
  }
}
