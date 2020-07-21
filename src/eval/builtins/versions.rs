use crate::{error::Result, eval::Eval, thunk::ThunkId, value::Value};
use std::cmp::Ordering;

pub async fn compare_versions(eval: &Eval, lhs: ThunkId, rhs: ThunkId) -> Result<Value> {
  let cmp = do_compare(
    &eval.value_string_of(lhs).await?,
    &eval.value_string_of(rhs).await?,
  );
  Ok(Value::Int(match cmp {
    Ordering::Less => -1,
    Ordering::Greater => 1,
    Ordering::Equal => 0,
  }))
}

fn do_compare(s1: &str, s2: &str) -> Ordering {
  let mut iter1 = s1.split(|p| p == '.' || p == '-');
  let mut iter2 = s2.split(|p| p == '.' || p == '-');
  loop {
    let num1 = iter1.next();
    let num2 = iter2.next();
    if num1.is_none() && num2.is_none() {
      break Ordering::Equal;
    }
    let c1 = num1.unwrap_or("");
    let c2 = num2.unwrap_or("");
    if components_lt(c1, c2) {
      break Ordering::Less;
    } else if components_lt(c2, c1) {
      break Ordering::Greater;
    }
  }
}

fn components_lt(s1: &str, s2: &str) -> bool {
  let num1 = s1.parse::<i64>().ok();
  let num2 = s2.parse::<i64>().ok();
  if let (Some(n1), Some(n2)) = (num1, num2) {
    n1 < n2
  } else if s1 == "" && num2.is_some() || s1 == "pre" && s2 != "pre" {
    true
  } else if s2 == "pre" {
    false
  } else if num1.is_some() {
    true
  } else if num2.is_some() {
    false
  } else {
    s1 < s2
  }
}
