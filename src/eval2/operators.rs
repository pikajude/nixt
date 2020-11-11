use super::*;
use crate::syntax::expr::{Binary, BinaryOp, Unary, UnaryOp};

impl Eval {
  pub fn binary(&self, bin: &Binary, context: &Context) -> Result<ValueRef> {
    macro_rules! v {
      ($x:expr) => {
        &self.defer($x, context)
      };
    }

    match bin.op.node {
      BinaryOp::Or => {
        if self.value_bool_of(v!(bin.lhs))? {
          Ok(arc(Value::Bool(true)))
        } else {
          self.eval(bin.rhs, context)
        }
      }
      BinaryOp::And => {
        if self.value_bool_of(v!(bin.lhs))? {
          self.eval(bin.rhs, context)
        } else {
          Ok(arc(Value::Bool(false)))
        }
      }
      BinaryOp::Eq => Ok(arc(Value::Bool(self.equals(v!(bin.lhs), v!(bin.rhs))?))),
      BinaryOp::Neq => Ok(arc(Value::Bool(!self.equals(v!(bin.lhs), v!(bin.rhs))?))),

      BinaryOp::Add => self.operator_add(v!(bin.lhs), v!(bin.rhs)),
      BinaryOp::Sub => self.operator_sub(v!(bin.lhs), v!(bin.rhs)),

      BinaryOp::Ge => Ok(arc(Value::Bool(self.less_than(v!(bin.rhs), v!(bin.lhs))?))),
      BinaryOp::Geq => Ok(arc(Value::Bool(!self.less_than(v!(bin.lhs), v!(bin.rhs))?))),
      BinaryOp::Le => Ok(arc(Value::Bool(self.less_than(v!(bin.lhs), v!(bin.rhs))?))),
      BinaryOp::Leq => Ok(arc(Value::Bool(!self.less_than(v!(bin.rhs), v!(bin.lhs))?))),

      BinaryOp::Impl => {
        let lhs = self.value_bool_of(v!(bin.lhs))?;
        Ok(arc(Value::Bool(!lhs || self.value_bool_of(v!(bin.rhs))?)))
      }
      BinaryOp::Update => {
        let mut lhs = self.value_attrs_of(v!(bin.lhs))?.clone();
        for (k, v) in self.value_attrs_of(v!(bin.rhs))?.iter() {
          lhs.insert(k.clone(), Arc::clone(v));
        }
        Ok(arc(Value::Attrs(lhs)))
      }
      x => bail!("unhandled binary operator {:?}", x),
    }
  }

  pub fn unary(&self, un: &Unary, context: &Context) -> Result<ValueRef> {
    macro_rules! v {
      ($x:expr) => {
        &self.defer($x, context)
      };
    }

    match un.op.node {
      UnaryOp::Not => Ok(arc(Value::Bool(!self.value_bool_of(v!(un.operand))?))),
      UnaryOp::Negate => bail!("- operator"),
    }
  }

  pub fn equals(&self, lhs: &ValueRef, rhs: &ValueRef) -> Result<bool> {
    if Arc::ptr_eq(lhs, rhs) {
      return Ok(true);
    }

    let lhs_val = self.value(lhs)?;
    let rhs_val = self.value(rhs)?;

    let lhs = &*lhs_val;
    let rhs = &*rhs_val;

    if let Value::Int(i) = lhs {
      if let Value::Float(f) = rhs {
        return Ok(*i == (*f as i64));
      }
    }

    if let Value::Int(i) = rhs {
      if let Value::Float(f) = lhs {
        return Ok(*i == (*f as i64));
      }
    }

    if std::mem::discriminant(lhs) != std::mem::discriminant(rhs) {
      return Ok(false);
    }

    Ok(match (&*lhs, &*rhs) {
      (Value::Int(i), Value::Int(i2)) => i == i2,
      (Value::Float(f), Value::Float(f2)) => (f - f2).abs() <= f64::EPSILON,
      (Value::String((s1, _)), Value::String((s2, _))) => s1 == s2,
      (Value::Path(p1), Value::Path(p2)) => p1 == p2,
      (Value::Null, _) => true,
      (Value::Bool(b), Value::Bool(b2)) => b == b2,
      (Value::List(l1), Value::List(l2)) => {
        if l1.len() != l2.len() {
          return Ok(false);
        }
        for (item1, item2) in l1.iter().zip(l2) {
          if !self.equals(item1, item2)? {
            return Ok(false);
          }
        }
        true
      }
      (Value::Attrs(a1), Value::Attrs(a2)) => {
        // if both values are derivations, compare their paths
        if let Some(o1) = a1.get(&Ident::from("outPath")) {
          if let Some(o2) = a2.get(&Ident::from("outPath")) {
            return Ok(Arc::ptr_eq(o1, o2));
          }
        }

        if a1.len() != a2.len() {
          return Ok(false);
        }

        for (k, v) in a1.iter() {
          if let Some(v2) = a2.get(k) {
            if !self.equals(v, v2)? {
              return Ok(false);
            }
          } else {
            return Ok(false);
          }
        }
        true
      }
      (Value::Lambda { .. } | Value::Primop(_), _)
      | (_, Value::Lambda { .. } | Value::Primop(_)) => false,
      (x, y) => bail!("cannot compare {} with {}", x.typename(), y.typename()),
    })
  }

  fn operator_add(&self, lhs: &ValueRef, rhs: &ValueRef) -> Result<ValueRef> {
    match (&*self.value(lhs)?, &*self.value(rhs)?) {
      (Value::Path(p), Value::Path(p2)) => Ok(arc(Value::Path(concat_paths(p, p2)))),
      (Value::Path(p), Value::String((string, ctx))) => {
        if ctx.is_empty() {
          Ok(arc(Value::Path(concat_paths(p, string))))
        } else {
          bail!("a string that refers to store paths cannot be appended to a path")
        }
      }

      (Value::Int(i), Value::Int(i2)) => Ok(arc(Value::Int(i + i2))),
      (Value::Int(i), Value::Float(f)) => Ok(arc(Value::Float(*i as f64 + f))),
      (Value::Int(_), v) => bail!("cannot add a value of type {} to an integer", v.typename()),

      (Value::Float(f), Value::Int(i2)) => Ok(arc(Value::Float(f + (*i2 as f64)))),
      (Value::Float(f), Value::Float(f2)) => Ok(arc(Value::Float(f + f2))),
      (Value::Float(_), v) => bail!("cannot add a value of type {} to a float", v.typename()),

      (v, _) => {
        let lhs_is_string = matches!(v, Value::String {..});
        let mut ctx = Default::default();
        let mut buf = String::new();
        buf.push_str(&self.coerce_to_string(
          lhs,
          &mut ctx,
          CoerceOpts {
            extended: false,
            copy_to_store: lhs_is_string,
          },
        )?);
        buf.push_str(&self.coerce_to_string(
          rhs,
          &mut ctx,
          CoerceOpts {
            extended: false,
            copy_to_store: lhs_is_string,
          },
        )?);
        Ok(arc(Value::String((buf, ctx))))
      }
    }
  }

  fn operator_sub(&self, lhs: &ValueRef, rhs: &ValueRef) -> Result<ValueRef> {
    Ok(arc(match (&*self.value(lhs)?, &*self.value(rhs)?) {
      (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 - f2),
      (Value::Float(f1), Value::Int(f2)) => Value::Float(*f1 - *f2 as f64),
      (Value::Int(f1), Value::Float(f2)) => Value::Float(*f1 as f64 - *f2),
      (Value::Int(f1), Value::Int(f2)) => Value::Int(f1 - f2),
      (Value::Float(_), v) => bail!("expected a float, got {}", v.typename()),
      (Value::Int(_), v) => bail!("expected an integer, got {}", v.typename()),
      (v, _) => bail!("expected an integer, got {}", v.typename()),
    }))
  }

  fn less_than(&self, lhs: &ValueRef, rhs: &ValueRef) -> Result<bool> {
    Ok(match (&*self.value(lhs)?, &*self.value(rhs)?) {
      (Value::Float(f1), Value::Int(i1)) => *f1 < (*i1 as f64),
      (Value::Int(i1), Value::Float(f1)) => (*i1 as f64) < *f1,
      (Value::Int(i1), Value::Int(i2)) => i1 < i2,
      (Value::Float(f1), Value::Float(f2)) => f1 < f2,
      (Value::String((s1, _)), Value::String((s2, _))) => s1 < s2,
      (Value::Path(p1), Value::Path(p2)) => p1 < p2,
      (v1, v2) => bail!("cannot compare {} with {}", v1.typename(), v2.typename()),
    })
  }
}
