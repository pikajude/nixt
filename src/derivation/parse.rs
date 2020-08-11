use super::*;

impl Derivation {
  pub fn parse<'a, S: Store + ?Sized>(store: &S, input: &'a str, name: &str) -> Result<Self> {
    parse_derivation(store, input, name)
  }
}

fn parse_derivation(store: &(impl Store + ?Sized), input: &str, name: &str) -> Result<Derivation> {
  let mut p = Parser { input };
  p.expect("Derive([")?;

  let mut outputs = BTreeMap::<String, Output>::new();

  while !p.end_of_list() {
    p.expect("(")?;
    let id = p.string()?;
    p.expect(",")?;
    let path = store.parse_store_path(&p.path()?)?;
    p.expect(",")?;
    let hash_algo = p.string()?;
    p.expect(",")?;
    let hash = p.string()?;
    p.expect(")")?;
    outputs.insert(
      id,
      Output {
        path,
        hash: if !hash_algo.is_empty() && !hash.is_empty() {
          todo!("fixed output derivation")
        } else {
          None
        },
      },
    );
  }

  let mut input_drvs = BTreeMap::<StorePath, BTreeSet<String>>::new();

  p.expect(",[")?;
  while !p.end_of_list() {
    p.expect("(")?;
    let drvpath = p.path()?;
    p.expect(",[")?;
    input_drvs.insert(
      store.parse_store_path(&drvpath)?,
      p.strings()?.into_iter().collect(),
    );
    p.expect(")")?;
  }

  p.expect(",[")?;
  let input_srcs = p
    .paths()?
    .into_iter()
    .map(|x| store.parse_store_path(&x))
    .collect::<Result<BTreeSet<StorePath>>>()?;

  p.expect(",")?;
  let platform = p.string()?;
  p.expect(",")?;
  let builder = p.string()?;

  p.expect(",[")?;
  let args = p.strings()?;

  let mut env = BTreeMap::new();
  p.expect(",[")?;
  while !p.end_of_list() {
    p.expect("(")?;
    let name = p.string()?;
    p.expect(",")?;
    let value = p.string()?;
    p.expect(")")?;
    env.insert(name, value);
  }

  p.expect(")")?;
  Ok(Derivation {
    name: name.into(),
    builder: builder.into(),
    platform,
    args,
    env,
    input_sources: input_srcs,
    outputs,
    input_derivations: input_drvs,
  })
}

struct Parser<'a> {
  input: &'a str,
}

impl<'a> Parser<'a> {
  fn next(&mut self) -> Option<char> {
    if let Some(x) = self.peek() {
      self.input = &self.input[x.len_utf8()..];
      Some(x)
    } else {
      None
    }
  }

  fn next_ensure(&mut self) -> Result<char> {
    self.next().ok_or_else(|| anyhow!("unexpected EOF"))
  }

  fn peek(&self) -> Option<char> {
    self.input.chars().next()
  }

  fn expect(&mut self, lit: &str) -> Result<()> {
    if let Some(i) = self.input.strip_prefix(lit) {
      self.input = i;
      Ok(())
    } else {
      bail!("expected string {:?}", lit);
    }
  }

  fn end_of_list(&mut self) -> bool {
    let p = self.peek();
    if p == Some(',') {
      self.next();
      false
    } else if p == Some(']') {
      self.next();
      true
    } else {
      false
    }
  }

  fn string(&mut self) -> Result<String> {
    self.expect("\"")?;
    let mut out = String::new();
    while let Some(x) = self.next() {
      if x == '"' {
        break;
      }
      if x == '\\' {
        match self.next_ensure()? {
          'n' => out.push('\n'),
          'r' => out.push('\r'),
          't' => out.push('\t'),
          x => out.push(x),
        }
      } else {
        out.push(x);
      }
    }
    Ok(out)
  }

  fn path(&mut self) -> Result<PathBuf> {
    let s = self.string()?;
    ensure!(s.starts_with('/'), "path is invalid: {:?}", s);
    Ok(PathBuf::from(s))
  }

  fn strings(&mut self) -> Result<Vec<String>> {
    let mut out = vec![];
    while !self.end_of_list() {
      out.push(self.string()?);
    }
    Ok(out)
  }

  fn paths(&mut self) -> Result<Vec<PathBuf>> {
    let mut out = vec![];
    while !self.end_of_list() {
      out.push(self.path()?);
    }
    Ok(out)
  }
}
