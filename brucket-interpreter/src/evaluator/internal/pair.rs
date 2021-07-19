use crate::evaluator::ValueResult;
use crate::value::Value;
use std::borrow::Cow;
use std::collections::HashMap;

pub fn new(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[pair_new] Unknown variable: first")?;
    let second = environment
        .remove("second")
        .ok_or("[pair_new] Unknown variable: second")?;
    Ok(Value::Pair(Box::new(first), Box::new(second)))
}

pub fn first(mut environment: HashMap<String, Value>) -> ValueResult {
    let pair = environment
        .remove("pair")
        .ok_or("[pair_first] Unknown variable: pair")?;
    if let Value::Pair(first, _) = pair {
        Ok(*first)
    } else {
        Err(Cow::from("Invalid type of argument, expected: Pair"))
    }
}

pub fn second(mut environment: HashMap<String, Value>) -> ValueResult {
    let pair = environment
        .remove("pair")
        .ok_or("[pair_second] Unknown variable: pair")?;
    if let Value::Pair(_, second) = pair {
        Ok(*second)
    } else {
        Err(Cow::from("Invalid type of argument, expected: Pair"))
    }
}
