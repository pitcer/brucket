use crate::evaluator::{Value, ValueResult};
use crate::value::Numeric;
use derive_more::Constructor;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Rem, Sub};

mod pair;
#[cfg(test)]
mod tests;

pub type InternalFunction = fn(HashMap<String, Value>) -> ValueResult;

#[derive(Default, Clone, Constructor)]
pub struct InternalEnvironment;

impl InternalEnvironment {
    pub fn get(identifier: &str) -> Option<InternalFunction> {
        match identifier {
            "add_internal" => Some(add),
            "subtract_internal" => Some(subtract),
            "multiply_internal" => Some(multiply),
            "divide_internal" => Some(divide),
            "remainder_internal" => Some(remainder),
            "is_equal_internal" => Some(is_equal),
            "is_greater_internal" => Some(is_greater),
            "is_greater_or_equal_internal" => Some(is_greater_or_equal),
            "is_less_internal" => Some(is_less),
            "is_less_or_equal_internal" => Some(is_less_or_equal),
            "pair_new_internal" => Some(pair::new),
            "pair_first_internal" => Some(pair::first),
            "pair_second_internal" => Some(pair::second),
            _ => None
        }
    }
}

macro_rules! implement_arithmetic_operation {
    ($type:ty, $operation:ident) => {
        impl $type for Numeric {
            type Output = Self;

            #[inline]
            fn $operation(self, rhs: Numeric) -> Self::Output {
                match self {
                    Numeric::Integer(first) => match rhs {
                        Numeric::Integer(second) => Numeric::Integer(first.$operation(second)),
                        Numeric::FloatingPoint(second) => {
                            Numeric::FloatingPoint((first as f64).$operation(second))
                        }
                    },
                    Numeric::FloatingPoint(first) => match rhs {
                        Numeric::Integer(second) => {
                            Numeric::FloatingPoint(first.$operation(second as f64))
                        }
                        Numeric::FloatingPoint(second) => {
                            Numeric::FloatingPoint(first.$operation(second))
                        }
                    },
                }
            }
        }
    };
}

implement_arithmetic_operation!(Add, add);
implement_arithmetic_operation!(Sub, sub);
implement_arithmetic_operation!(Mul, mul);
implement_arithmetic_operation!(Div, div);
implement_arithmetic_operation!(Rem, rem);

fn add(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[add] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[add] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Numeric(first + second))
}

fn subtract(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[subtract] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[subtract] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Numeric(first - second))
}

fn multiply(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[multiply] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[multiply] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Numeric(first * second))
}

fn divide(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[divide] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[divide] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Numeric(first / second))
}

fn remainder(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[remainder] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[remainder] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Numeric(first % second))
}

fn is_equal(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[is_equal] Unknown variable: first")?;
    let second = environment
        .remove("second")
        .ok_or("[is_equal] Unknown variable: second")?;
    Ok(Value::Boolean(first == second))
}

fn is_greater(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[is_greater] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[is_greater] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Boolean(first > second))
}

fn is_greater_or_equal(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[is_greater_or_equal] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[is_greater_or_equal] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Boolean(first >= second))
}

fn is_less(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[is_less] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[is_less] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Boolean(first < second))
}

fn is_less_or_equal(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[is_less_or_equal] Unknown variable: first")?
        .into_numeric()?;
    let second = environment
        .remove("second")
        .ok_or("[is_less_or_equal] Unknown variable: second")?
        .into_numeric()?;
    Ok(Value::Boolean(first <= second))
}

impl Value {
    fn into_numeric(self) -> Result<Numeric, &'static str> {
        match self {
            Value::Numeric(value) => Ok(value),
            _ => Err("Invalid argument type"),
        }
    }
}
