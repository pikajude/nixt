use derive_more::Display;
use std::borrow::ToOwned;

#[derive(Debug, Clone, Copy, Display)]
#[display(fmt = "{}", node)]
pub struct Spanned<T> {
  pub node: T,
  pub span: FileSpan,
}

pub fn spanned<T>(span: FileSpan, node: T) -> Spanned<T> {
  Spanned::new(span, node)
}

impl<T: ToOwned> Spanned<T> {
  pub fn owned(&self) -> Spanned<T::Owned> {
    Spanned::new(self.span, self.node.to_owned())
  }
}

impl<T: Clone> Spanned<&'_ T> {
  pub fn cloned(self) -> Spanned<T> {
    Spanned::new(self.span, self.node.clone())
  }
}

impl<T> std::ops::Deref for Spanned<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.node
  }
}

impl<T> std::ops::DerefMut for Spanned<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.node
  }
}

impl<T> Spanned<T> {
  pub fn new(span: FileSpan, node: T) -> Self {
    Self { node, span }
  }

  pub fn borrowed(&self) -> Spanned<&T> {
    Spanned {
      span: self.span,
      node: &self.node,
    }
  }

  pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
    Spanned {
      node: f(self.node),
      span: self.span,
    }
  }

  pub fn replace<U>(&self, v: U) -> Spanned<U> {
    Spanned {
      node: v,
      span: self.span,
    }
  }

  pub fn try_map<E, U, F: FnOnce(T) -> Result<U, E>>(self, f: F) -> Result<Spanned<U>, E> {
    let s = self.span;
    f(self.node).map(|x| Spanned { span: s, node: x })
  }

  pub fn def<U: Default>(&self) -> Spanned<U> {
    self.replace(Default::default())
  }

  // pub fn label(&self, m: impl Into<String>) -> Label {
  //   Label::new(self.span.file_id, self.span.span, m)
  // }
}

#[derive(Debug, Clone, Copy)]
pub struct FileSpan {
  pub span: codespan::Span,
  pub file_id: codespan::FileId,
}

impl FileSpan {
  pub fn new(
    a: impl Into<codespan::ByteIndex>,
    b: impl Into<codespan::ByteIndex>,
    file_id: codespan::FileId,
  ) -> Self {
    Self {
      span: codespan::Span::new(a, b),
      file_id,
    }
  }

  pub fn merge(&self, other: &Self) -> Self {
    if self.file_id != other.file_id {
      panic!("file id mismatch")
    }

    FileSpan {
      file_id: self.file_id,
      span: self.span.merge(other.span),
    }
  }
}
