pub mod source_tree;

use anyhow::Result;
use arena::{Arena, Id};
use source_tree::Source;
use std::path::Path;
use syntax::expr::ExprRef;

#[derive(Debug)]
pub enum Value {}

pub type ThunkId = Id<Thunk>;

#[derive(Debug)]
pub enum Thunk {
  H(ExprRef),
  V(Value),
  Blackhole,
}

pub struct Eval {
  allocator: Arena<Thunk>,
  source: Source,
}

impl Eval {
  pub fn new() -> Self {
    Self {
      allocator: Arena::new(),
      source: Source::new(),
    }
  }

  pub fn source(&self) -> &Source {
    &self.source
  }

  pub async fn load<P: AsRef<Path>>(&mut self, file: P) -> Result<ThunkId> {
    let exprid = self.source.load_file(file).await?;
    Ok(self.allocator.insert(Thunk::H(exprid)))
  }

  pub fn force(&mut self, id: ThunkId) -> Result<()> {
    let expr_id = match self.allocator[id] {
      Thunk::Blackhole => panic!("Infinite recursion"),
      Thunk::V(_) => return Ok(()),
      Thunk::H(e) => {
        self.allocator[id] = Thunk::Blackhole;
        e
      }
    };

    eprintln!("{:?}", self.source.expr(*expr_id));

    Ok(())
  }
}

impl Default for Eval {
  fn default() -> Self {
    Self::new()
  }
}
