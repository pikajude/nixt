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
}
