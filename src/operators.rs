use crate::{
  bail,
  error::Result,
  thunk::{Context, Thunk},
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
    BinaryOp::Eq => {
      let lhs = eval.value_of(t!(bin.lhs)).await?;
      let rhs = eval.value_of(t!(bin.rhs)).await?;

      Ok(Value::Bool(eval_eq(eval, lhs, rhs).await?))
    }
    BinaryOp::Impl => {
      let lhs = eval.value_bool_of(t!(bin.lhs)).await?;
      let rhs = eval.value_bool_of(t!(bin.rhs)).await?;
      Ok(Value::Bool(!lhs || rhs))
    }
    BinaryOp::Update => {
      let mut lhs = eval.value_attrs_of(t!(bin.lhs)).await?.clone();
      for (k, v) in eval.value_attrs_of(t!(bin.rhs)).await? {
        lhs.insert(k.clone(), *v);
      }
      Ok(Value::AttrSet(lhs))
    }
    x => bail!("unimplemented: {:?}", x),
  }
}

#[async_recursion]
async fn eval_eq(eval: &Eval, lhs: &Value, rhs: &Value) -> Result<bool> {
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
    (Value::Lambda { .. }, Value::Primop(_)) => false,
    (x, y) => bail!("cannot compare {} with {}", x.typename(), y.typename()),
  })
}

pub async fn eval_unary(eval: &Eval, un: &Unary, context: Context) -> Result<Value> {
  match *un.op {
    UnaryOp::Not => Ok(Value::Bool(
      !eval
        .value_bool_of(eval.items.alloc(Thunk::thunk(un.operand, context.clone())))
        .await?,
    )),
    x => bail!("unimplemented: {:?}", x),
  }
}
