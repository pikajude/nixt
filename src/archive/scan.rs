use crate::prelude::*;
use std::collections::HashSet;

pub struct RefsScanner {
  hashes: HashSet<Vec<u8>>,
  pub seen: HashSet<Vec<u8>>,
  tail: Vec<u8>,
}

impl RefsScanner {
  pub fn new(hashes: impl IntoIterator<Item = PathHash>) -> Self {
    Self {
      hashes: hashes
        .into_iter()
        .map(|h| h.to_string().as_bytes().to_vec())
        .collect(),
      seen: HashSet::new(),
      tail: vec![],
    }
  }
}

const HASH_LENGTH: usize = 32;

fn search(data: &[u8], hashes: &mut HashSet<Vec<u8>>, seen: &mut HashSet<Vec<u8>>) {
  if hashes.is_empty() {
    return;
  }

  let mut i = 0;
  while i + HASH_LENGTH <= data.len() {
    let mut matched = true;
    let mut j = HASH_LENGTH - 1;
    while j > 0 {
      if !crate::util::base32::IS_BASE32[data[i + j] as usize] {
        i += j + 1;
        matched = false;
        break;
      }
      j -= 1;
    }
    if !matched {
      continue;
    }
    let this_ref = &data[i..i + HASH_LENGTH];
    if hashes.remove(this_ref) {
      seen.insert(this_ref.to_vec());
    }
    i += 1;
  }
}

impl io::Write for RefsScanner {
  fn write(&mut self, data: &[u8]) -> io::Result<usize> {
    // all hashes found, this scanner is now a no-op
    if self.hashes.is_empty() {
      return Ok(data.len());
    }

    let len = data.len();

    self.tail.extend(if len > HASH_LENGTH {
      &data[..HASH_LENGTH]
    } else {
      data
    });

    search(self.tail.as_slice(), &mut self.hashes, &mut self.seen);
    search(data, &mut self.hashes, &mut self.seen);

    let tail_len = if len <= HASH_LENGTH { len } else { HASH_LENGTH };
    let tail_start = if self.tail.len() < HASH_LENGTH - tail_len {
      0
    } else {
      self.tail.len() - (HASH_LENGTH - tail_len)
    };
    self.tail = self.tail.split_off(tail_start);
    self.tail.extend(&data[len - tail_len..]);

    Ok(len)
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}
