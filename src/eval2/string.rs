use super::*;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct CoerceOpts {
  pub extended: bool,
  pub copy_to_store: bool,
}

impl CoerceOpts {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn extended(mut self) -> Self {
    self.extended = true;
    self
  }

  pub fn dont_copy(mut self) -> Self {
    self.copy_to_store = false;
    self
  }
}

impl Default for CoerceOpts {
  fn default() -> Self {
    Self {
      extended: false,
      copy_to_store: true,
    }
  }
}

impl Eval {
  pub fn coerce_to_string(
    &self,
    v: &ValueRef,
    context: &mut BTreeSet<String>,
    opts: CoerceOpts,
  ) -> Result<String> {
    Ok(match &*self.value(v)? {
      Value::Null if opts.extended => String::new(),
      Value::Bool(b) if opts.extended => {
        if *b {
          "1".into()
        } else {
          "".into()
        }
      }
      Value::Int(i) => i.to_string(),
      Value::List(l) if opts.extended => {
        let mut output = String::new();
        for (i, item) in l.iter().enumerate() {
          if i > 0 {
            output.push(' ');
          }
          output.push_str(&self.coerce_to_string(item, context, opts)?);
        }
        output
      }
      Value::Attrs(a) => {
        if let Some(o) = a.get(&Ident::from("outPath")) {
          self.coerce_to_string(o, context, opts)?
        } else {
          bail!("cannot coerce a set to a string unless it has an `outPath` attribute")
        }
      }
      Value::String(StringCtx { s, context: ctx }) => {
        context.extend(ctx.iter().cloned());
        s.clone()
      }
      v => bail!("cannot convert {} to a string", v.typename()),
    })
  }
}
