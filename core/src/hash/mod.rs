use crypto::digest::Digest;
use nix_util::*;
use std::{
  borrow::Cow,
  fmt::{self, Debug},
  hash::Hasher,
  str::FromStr,
};

mod context;
mod sink;

pub use context::Context;
pub use sink::Sink;

#[derive(Debug, thiserror::Error)]
pub enum Error {
  #[error("incorrect length `{0}' for hash")]
  WrongHashLen(usize),
  #[error("attempt to parse untyped hash `{0}'")]
  UntypedHash(String),
  #[error("unknown hash type `{0}'")]
  UnknownHashType(String),
  #[error("empty hash requires explicit type")]
  UntypedEmptyHash,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
pub enum HashType {
  #[display(fmt = "md5")]
  MD5,
  #[display(fmt = "sha1")]
  SHA1,
  #[display(fmt = "sha256")]
  SHA256,
  #[display(fmt = "sha512")]
  SHA512,
}

impl HashType {
  fn size(self) -> usize {
    match self {
      Self::MD5 => 16,
      Self::SHA1 => 20,
      Self::SHA256 => 32,
      Self::SHA512 => 64,
    }
  }
}

impl FromStr for HashType {
  type Err = Error;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(match s {
      "md5" => Self::MD5,
      "sha1" => Self::SHA1,
      "sha256" => Self::SHA256,
      "sha512" => Self::SHA512,
      x => return Err(Error::UnknownHashType(x.into())),
    })
  }
}

#[derive(Clone)]
pub struct Hash {
  data: [u8; 64],
  len: usize,
  ty: HashType,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Encoding {
  Base64,
  Base32,
  Base16,
  SRI,
}

impl Hash {
  fn len_base16(&self) -> usize {
    len_base16(self.len)
  }

  fn len_base32(&self) -> usize {
    len_base32(self.len)
  }

  fn len_base64(&self) -> usize {
    len_base64(self.len)
  }

  /// Size in bytes.
  pub fn size(&self) -> usize {
    self.len
  }

  /// Which algorithm produced this hash. See [`HashType`]
  pub fn type_(&self) -> HashType {
    self.ty
  }

  #[inline]
  pub fn as_bytes(&self) -> &[u8] {
    &self.data[..self.len]
  }

  pub fn new_allow_empty(s: &str, ty: Option<HashType>) -> Result<Self> {
    if s.is_empty() {
      if let Some(ht) = ty {
        let this = Self {
          data: [0; 64],
          len: ht.size(),
          ty: ht,
        };
        warn!(
          "found empty hash, assuming `{}'",
          this.encode_with_type(Encoding::SRI)
        );
        Ok(this)
      } else {
        bail!(Error::UntypedEmptyHash)
      }
    } else {
      match ty {
        Some(ht) => Self::decode_with_type(s, ht, false),
        None => Self::decode(s),
      }
    }
  }

  /// Encode to serialized representation
  pub fn encode(&self, encoding: Encoding) -> String {
    if encoding == Encoding::SRI {
      return self.encode_with_type(encoding);
    }
    let mut s = String::new();
    self.encode_impl(encoding, &mut s);
    s
  }

  pub fn encode_with_type(&self, encoding: Encoding) -> String {
    let mut s = self.ty.to_string();
    if encoding == Encoding::SRI {
      s.push('-');
    } else {
      s.push(':');
    }
    self.encode_impl(encoding, &mut s);
    s
  }

  fn encode_impl(&self, encoding: Encoding, buf: &mut String) {
    let bytes = match encoding {
      Encoding::Base16 => {
        let mut bytes = vec![0; self.len_base16()];
        binascii::bin2hex(self.as_bytes(), &mut bytes).expect("Incorrect buffer size");
        bytes
      }
      Encoding::Base32 => {
        let mut bytes = vec![0; self.len_base32()];
        crate::base32::encode_into(self.as_bytes(), &mut bytes);
        bytes
      }
      Encoding::Base64 | Encoding::SRI => {
        let mut bytes = vec![0; self.len_base64()];
        binascii::b64encode(self.as_bytes(), &mut bytes).expect("Incorrect buffer size");
        bytes
      }
    };
    buf.push_str(unsafe { std::str::from_utf8_unchecked(&bytes) });
  }

  /// Decode from serialized representation
  pub fn decode(input: &str) -> Result<Self> {
    if let Some((ty, rest)) = break_str(input, ':') {
      Ok(Self::decode_with_type(rest, ty.parse()?, false)?)
    } else if let Some((ty, rest)) = break_str(input, '-') {
      Ok(Self::decode_with_type(rest, ty.parse()?, true)?)
    } else {
      bail!(Error::UntypedHash(input.into()))
    }
  }

  pub fn decode_with_type(input: &str, ty: HashType, sri: bool) -> Result<Self> {
    let mut bytes = [0; 64];
    if !sri && input.len() == len_base16(ty.size()) {
      binascii::hex2bin(input.as_bytes(), &mut bytes).map_err(|e| anyhow::anyhow!("{:?}", e))?;
      Ok(Self {
        data: bytes,
        ty,
        len: ty.size(),
      })
    } else if !sri && input.len() == len_base32(ty.size()) {
      crate::base32::decode_into(input.as_bytes(), &mut bytes)?;
      Ok(Self {
        data: bytes,
        ty,
        len: ty.size(),
      })
    } else {
      todo!()
    }
  }

  pub fn hash_bytes(data: &[u8], ty: HashType) -> Self {
    let mut ctx = Context::new(ty);
    ctx.input(data);
    ctx.finish().0
  }

  pub fn hash_str(data: &str, ty: HashType) -> Self {
    Self::hash_bytes(data.as_bytes(), ty)
  }

  /// Convert `self` to a shorter hash by recursively XOR-ing bytes.
  pub fn truncate(&self, new_size: usize) -> Cow<Self> {
    if new_size >= self.len {
      return Cow::Borrowed(self);
    }
    let mut data = [0; 64];
    for i in 0..self.len {
      data[i % new_size] ^= self.data[i];
    }
    Cow::Owned(Self {
      len: new_size,
      data,
      ty: self.ty,
    })
  }
}

fn len_base16(size: usize) -> usize {
  size * 2
}

fn len_base32(size: usize) -> usize {
  (size * 8 - 1) / 5 + 1
}

fn len_base64(size: usize) -> usize {
  ((4 * size / 3) + 3) & !3
}

impl PartialEq for Hash {
  fn eq(&self, other: &Self) -> bool {
    self.ty == other.ty && self.as_bytes() == other.as_bytes()
  }
}

impl Eq for Hash {}

impl std::hash::Hash for Hash {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.as_bytes().hash(state)
  }
}

impl Debug for Hash {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_tuple("Hash")
      .field(&format!("{}:{}", self.ty, self.encode(Encoding::Base64)))
      .finish()
  }
}
