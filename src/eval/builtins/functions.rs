use crate::{
  eval::{thunk::ThunkId, value::Value, Eval},
  syntax::expr::LambdaArg,
  util::*,
};

pub async fn function_args(eval: &Eval, fun: ThunkId) -> Result<Value> {
  let l = match eval.value_of(fun).await? {
    Value::Lambda { lambda, .. } => lambda,
    v => bail!("cannot call functionArgs on {}", v.typename()),
  };

  let mut fn_args = vec![];

  match &*l.argument {
    LambdaArg::Plain(_) => {}
    LambdaArg::Formals(fs) => {
      for arg in &fs.args {
        fn_args.push((arg.arg_name.node.clone(), arg.fallback.is_some()));
      }
    }
  }

  Ok(Value::AttrSet(
    fn_args
      .into_iter()
      .map(|(arg, req)| (arg, eval.new_value(Value::Bool(req))))
      .collect(),
  ))
}
