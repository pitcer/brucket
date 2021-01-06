/*
 * MIT License
 *
 * Copyright (c) 2020 Piotr Dobiech
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

use std::collections::HashMap;

use crate::evaluator::{Value, ValueResult};
use crate::value::Numeric;

use std::ops::{Add, Div, Mul, Rem, Sub};

#[cfg(test)]
mod test;

macro_rules! internal_environment_map {
    ($($identifier:literal => $function:expr),*) => {
        {
            let mut map = InternalEnvironmentMap::new();
            $(
                map.insert($identifier, $function);
            )*
            map.shrink_to_fit();
            map
        }
    };
}

pub type InternalFunction = fn(HashMap<String, Value>) -> ValueResult;
pub type InternalEnvironmentMap = HashMap<&'static str, InternalFunction>;

pub struct InternalEnvironment {
    map: InternalEnvironmentMap,
}

impl Default for InternalEnvironment {
    fn default() -> Self {
        let map = internal_environment_map! {
            "add_internal" => add,
            "subtract_internal" => subtract,
            "multiply_internal" => multiply,
            "divide_internal" => divide,
            "remainder_internal" => remainder,
            "is_equal_internal" => is_equal,
            "is_greater_internal" => is_greater,
            "is_greater_or_equal_internal" => is_greater_or_equal,
            "is_less_internal" => is_less,
            "is_less_or_equal_internal" => is_less_or_equal,
            "pair_new_internal" => pair::new,
            "pair_first_internal" => pair::first,
            "pair_second_internal" => pair::second
        };
        Self::new(map)
    }
}

impl InternalEnvironment {
    fn new(map: InternalEnvironmentMap) -> Self {
        Self { map }
    }

    pub fn get(&mut self, identifier: &str) -> Option<&InternalFunction> {
        self.map.get(identifier)
    }
}

macro_rules! implement_arithmetic_operation {
    ($type:ty, $operation:ident) => {
        impl $type for Numeric {
            type Output = Self;

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

mod pair {
    use super::*;
    use std::borrow::Cow;

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
}

impl Value {
    fn into_numeric(self) -> Result<Numeric, &'static str> {
        match self {
            Value::Numeric(value) => Ok(value),
            _ => Err("Invalid argument type"),
        }
    }
}
