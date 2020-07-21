use crate::{bail, error::Result};

use super::{
  builtins::strings::coerce_to_string,
  thunk::{Context, Thunk, ThunkId},
  value::Value,
  Eval,
};

use syntax::expr::{Binary, BinaryOp, Unary, UnaryOp};

pub async fn eval_binary(eval: &Eval, bin: &Binary, context: Context) -> Result<Value> {
  macro_rules! t {
    ($x:expr) => {
      eval.items.alloc(Thunk::thunk($x, context.clone()))
    };
  }

  match *bin.op {
    BinaryOp::Or => {
      if eval.value_bool_of(t!(bin.lhs)).await? {
        Ok(Value::Bool(true))
      } else {
        eval.step_eval(bin.rhs, context).await
      }
    }
    BinaryOp::And => {
      if eval.value_bool_of(t!(bin.lhs)).await? {
        eval.step_eval(bin.rhs, context).await
      } else {
        Ok(Value::Bool(false))
      }
    }
    BinaryOp::Add => plus_operator(eval, t!(bin.lhs), t!(bin.rhs)).await,
    BinaryOp::Sub => {
      let lhs = eval.value_of(t!(bin.lhs)).await?;
      let rhs = eval.value_of(t!(bin.rhs)).await?;

      do_sub(lhs, rhs)
    }
    BinaryOp::Eq => {
      let lhs = eval.value_of(t!(bin.lhs)).await?;
      let rhs = eval.value_of(t!(bin.rhs)).await?;

      Ok(Value::Bool(eval_eq(eval, lhs, rhs).await?))
    }
    BinaryOp::Neq => {
      let lhs = eval.value_of(t!(bin.lhs)).await?;
      let rhs = eval.value_of(t!(bin.rhs)).await?;

      Ok(Value::Bool(!eval_eq(eval, lhs, rhs).await?))
    }
    BinaryOp::Leq => {
      let lhs = eval.value_of(t!(bin.lhs)).await?;
      let rhs = eval.value_of(t!(bin.rhs)).await?;

      Ok(Value::Bool(!less_than(rhs, lhs)?))
    }
    BinaryOp::Le => {
      let lhs = eval.value_of(t!(bin.lhs)).await?;
      let rhs = eval.value_of(t!(bin.rhs)).await?;

      Ok(Value::Bool(less_than(lhs, rhs)?))
    }
    BinaryOp::Ge => {
      let lhs = eval.value_of(t!(bin.lhs)).await?;
      let rhs = eval.value_of(t!(bin.rhs)).await?;

      Ok(Value::Bool(less_than(rhs, lhs)?))
    }
    BinaryOp::Impl => {
      let lhs = eval.value_bool_of(t!(bin.lhs)).await?;
      Ok(Value::Bool(!lhs || eval.value_bool_of(t!(bin.rhs)).await?))
    }
    BinaryOp::Update => {
      let mut lhs = eval.value_attrs_of(t!(bin.lhs)).await?.clone();
      // trace!("{:?}", lhs.keys().collect::<Vec<_>>());
      for (k, v) in eval.value_attrs_of(t!(bin.rhs)).await? {
        lhs.insert(k.clone(), *v);
      }
      Ok(Value::AttrSet(lhs))
    }
    BinaryOp::Concat => {
      let mut lhs = eval.value_list_of(t!(bin.lhs)).await?.to_vec();
      let rhs = eval.value_list_of(t!(bin.rhs)).await?;
      lhs.extend(rhs);
      Ok(Value::List(lhs))
    }
    x => bail!("unimplemented: {:?}", x),
  }
}

fn do_sub(lhs: &Value, rhs: &Value) -> Result<Value> {
  match (lhs, rhs) {
    (Value::Float(f1), Value::Float(f2)) => Ok(Value::Float(f1 - f2)),
    (Value::Float(f1), Value::Int(f2)) => Ok(Value::Float(*f1 - *f2 as f64)),
    (Value::Int(f1), Value::Float(f2)) => Ok(Value::Float(*f1 as f64 - *f2)),
    (Value::Int(f1), Value::Int(f2)) => Ok(Value::Int(f1 - f2)),
    (Value::Float(_), v) => bail!("expected a float, got {}", v.typename()),
    (Value::Int(_), v) => bail!("expected an integer, got {}", v.typename()),
    (v, _) => bail!("expected an integer, got {}", v.typename()),
  }
}

#[async_recursion::async_recursion]
pub async fn eval_eq(eval: &Eval, lhs: &Value, rhs: &Value) -> Result<bool> {
  if lhs as *const _ == rhs as *const _ {
    return Ok(true);
  }

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

  Ok(match (lhs, rhs) {
    (Value::Int(i), Value::Int(i2)) => i == i2,
    (Value::Float(f), Value::Float(f2)) => (f - f2).abs() <= f64::EPSILON,
    (Value::String { string: s1, .. }, Value::String { string: s2, .. }) => s1 == s2,
    (Value::Path(p1), Value::Path(p2)) => p1 == p2,
    (Value::Null, _) => true,
    (Value::List(l1), Value::List(l2)) => {
      if l1.len() != l2.len() {
        return Ok(false);
      }
      for (item1, item2) in l1.iter().zip(l2) {
        let i1 = eval.value_of(*item1).await?;
        let i2 = eval.value_of(*item2).await?;
        if !eval_eq(eval, i1, i2).await? {
          return Ok(false);
        }
      }
      true
    }
    (Value::AttrSet(a1), Value::AttrSet(a2)) => {
      if a1.len() != a2.len() {
        return Ok(false);
      }
      for (k, v) in a1.iter() {
        if let Some(v2) = a2.get(k) {
          let v1_value = eval.value_of(*v).await?;
          let v2_value = eval.value_of(*v2).await?;
          if !eval_eq(eval, v1_value, v2_value).await? {
            return Ok(false);
          }
        } else {
          return Ok(false);
        }
      }
      true
    }
    (Value::Lambda { .. }, _)
    | (Value::Primop(_), _)
    | (_, Value::Lambda { .. })
    | (_, Value::Primop(_)) => false,
    (x, y) => bail!("cannot compare {} with {}", x.typename(), y.typename()),
  })
}

pub async fn eval_unary(eval: &Eval, un: &Unary, context: Context) -> Result<Value> {
  match *un.op {
    UnaryOp::Not => Ok(Value::Bool(
      !eval
        .value_bool_of(eval.items.alloc(Thunk::thunk(un.operand, context)))
        .await?,
    )),
    UnaryOp::Negate => do_sub(
      &Value::Int(0),
      eval
        .value_of(eval.items.alloc(Thunk::thunk(un.operand, context)))
        .await?,
    ),
  }
}

async fn plus_operator(eval: &Eval, lhs: ThunkId, rhs: ThunkId) -> Result<Value> {
  match eval.value_of(lhs).await? {
    Value::Path(p) => {
      let pathstr = eval.value_string_of(rhs).await?;
      Ok(Value::Path(p.join(pathstr)))
    }
    Value::Int(i) => match eval.value_of(rhs).await? {
      Value::Int(i2) => Ok(Value::Int(i + i2)),
      Value::Float(f) => Ok(Value::Float(*i as f64 + f)),
      v => bail!("cannot add value {} to an integer", v.typename()),
    },
    Value::Float(f) => match eval.value_of(rhs).await? {
      Value::Int(i2) => Ok(Value::Float(f + (*i2 as f64))),
      Value::Float(f2) => Ok(Value::Float(f + f2)),
      v => bail!("cannot add value {} to a float", v.typename()),
    },
    _ => concat_strings(eval, lhs, rhs).await,
  }
}

pub fn less_than(lhs: &Value, rhs: &Value) -> Result<bool> {
  Ok(match (lhs, rhs) {
    (Value::Float(f1), Value::Int(i1)) => *f1 < (*i1 as f64),
    (Value::Int(i1), Value::Float(f1)) => (*i1 as f64) < *f1,
    (Value::Int(i1), Value::Int(i2)) => i1 < i2,
    (Value::Float(f1), Value::Float(f2)) => f1 < f2,
    (Value::String { string: s1, .. }, Value::String { string: s2, .. }) => s1 < s2,
    (Value::Path(p1), Value::Path(p2)) => p1 < p2,
    (v1, v2) => bail!("cannot compare {} with {}", v1.typename(), v2.typename()),
  })
}

pub async fn concat_strings(eval: &Eval, lhs: ThunkId, rhs: ThunkId) -> Result<Value> {
  Ok(match try_join!(eval.value_of(lhs), eval.value_of(rhs))? {
    (Value::Path(p1), Value::Path(p2)) => Value::Path(p1.join(p2)),
    (
      Value::Path(p1),
      Value::String {
        string: s2,
        context,
      },
    ) => {
      if context.is_empty() {
        Value::Path(p1.join(s2))
      } else {
        bail!("a string that refers to a store path cannot be appended to a path")
      }
    }
    (x, _) => {
      let lhs_is_string = match x {
        Value::String { .. } => true,
        _ => false,
      };
      let mut ctx = Default::default();
      let mut buf = String::new();
      buf.push_str(&coerce_to_string(eval, lhs, &mut ctx, false, lhs_is_string).await?);
      buf.push_str(&coerce_to_string(eval, rhs, &mut ctx, false, lhs_is_string).await?);
      Value::String {
        string: buf,
        context: ctx,
      }
    }
  })
}
