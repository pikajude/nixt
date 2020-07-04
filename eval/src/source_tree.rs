use anyhow::{Context, Result};
use arena::Arena;
use codespan::Files;
use codespan_reporting::files::Files as RFiles;
use std::{
  collections::HashMap,
  ffi::OsString,
  ops::Range,
  path::{Path, PathBuf},
};
use syntax::expr::{Expr, ExprId, ExprRef};
use tokio::fs;

#[derive(Default)]
pub struct Source {
  allocator: Arena<Expr>,
  pub files: Files<String>,
  path_cache: HashMap<PathBuf, ExprRef>,
}

impl Source {
  pub fn new() -> Self {
    Self::default()
  }

  pub async fn load_file<P: AsRef<Path>>(&mut self, path: P) -> Result<ExprRef> {
    let p = path.as_ref();
    if let Some(id) = self.path_cache.get(p) {
      return Ok(*id);
    }

    let file_contents = fs::read_to_string(&path)
      .await
      .with_context(|| format!("Unable to read file {}", p.display()))?;
    let new_id = self.load_inline(p, file_contents)?;

    self.path_cache.insert(p.into(), new_id);
    Ok(new_id)
  }

  pub fn load_inline<N: Into<OsString>, S: Into<String>>(
    &mut self,
    name: N,
    source: S,
  ) -> Result<ExprRef> {
    let file_id = self.files.add(name.into(), source.into());
    Ok(syntax::parse(
      file_id,
      &mut self.allocator,
      self.files.source(file_id),
    )?)
  }

  pub fn expr(&self, e: ExprId) -> &Expr {
    &self.allocator[e]
  }
}

impl<'a> RFiles<'a> for Source {
  type FileId = <Files<String> as RFiles<'a>>::FileId;
  type Name = <Files<String> as RFiles<'a>>::Name;
  type Source = <Files<String> as RFiles<'a>>::Source;

  fn name(&'a self, id: Self::FileId) -> Option<Self::Name> {
    RFiles::name(&self.files, id)
  }

  fn source(&'a self, id: Self::FileId) -> Option<Self::Source> {
    RFiles::source(&self.files, id)
  }

  fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Option<usize> {
    RFiles::line_index(&self.files, id, byte_index)
  }

  fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Option<Range<usize>> {
    RFiles::line_range(&self.files, id, line_index)
  }
}
