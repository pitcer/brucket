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

use crate::evaluator::{Value, ValueResult};
use std::collections::HashMap;

macro_rules! internal_environment_map {
    ($($identifier:expr => $function:expr),*) => {
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

type InternalFunction = fn(Vec<Value>) -> ValueResult;
type InternalEnvironmentMap = HashMap<&'static str, InternalFunction>;

pub struct InternalEnvironment {
    map: InternalEnvironmentMap,
}

impl Default for InternalEnvironment {
    fn default() -> Self {
        let map = internal_environment_map! {
            "add" => add,
            "subtract" => subtract,
            "multiply" => multiply,
            "divide" => divide,
            "remainder" => remainder,
            "is_equal" => is_equal,
            "is_greater" => is_greater,
            "is_greater_or_equal" => is_greater_or_equal,
            "is_less" => is_less,
            "is_less_or_equal" => is_less_or_equal,
            "pair_new" => pair::new,
            "pair_first" => pair::first,
            "pair_second" => pair::second

        };
        Self::new(map)
    }
}

impl InternalEnvironment {
    fn new(map: InternalEnvironmentMap) -> Self {
        Self { map }
    }

    pub fn get(&self, identifier: &str) -> Option<&InternalFunction> {
        self.map.get(identifier)
    }
}

fn add(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first + second))
}

fn subtract(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first - second))
}

fn multiply(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first * second))
}

fn divide(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first / second))
}

fn remainder(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first % second))
}

fn is_equal(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    Ok(Value::Boolean(first == second))
}

fn is_greater(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Boolean(first > second))
}

fn is_greater_or_equal(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Boolean(first >= second))
}

fn is_less(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Boolean(first < second))
}

fn is_less_or_equal(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Boolean(first <= second))
}

mod pair {
    use super::*;

    pub fn new(arguments: Vec<Value>) -> ValueResult {
        let (first, second) = get_binary_function_arguments(arguments)?;
        Ok(Value::Pair(Box::new(first), Box::new(second)))
    }

    pub fn first(arguments: Vec<Value>) -> ValueResult {
        let argument = get_unary_function_argument(arguments)?;
        if let Value::Pair(first, _) = argument {
            Ok(*first)
        } else {
            Err("Invalid type of argument".to_string())
        }
    }

    pub fn second(arguments: Vec<Value>) -> ValueResult {
        let argument = get_unary_function_argument(arguments)?;
        if let Value::Pair(_, second) = argument {
            Ok(*second)
        } else {
            Err("Invalid type of argument".to_string())
        }
    }
}

fn get_unary_function_argument(arguments: Vec<Value>) -> Result<Value, String> {
    validate_arguments_length(&arguments, 1)?;
    let mut iterator = arguments.into_iter();
    let first = iterator.next().unwrap();
    Ok(first)
}

fn get_binary_function_arguments(arguments: Vec<Value>) -> Result<(Value, Value), String> {
    validate_arguments_length(&arguments, 2)?;
    let mut iterator = arguments.into_iter();
    let first = iterator.next().unwrap();
    let second = iterator.next().unwrap();
    Ok((first, second))
}

fn validate_arguments_length(arguments: &[Value], expected_length: usize) -> Result<(), String> {
    let actual_length = arguments.len();
    if actual_length == expected_length {
        Ok(())
    } else {
        Err(format!(
            "Invalid number of arguments. Expected: {}; Actual: {}",
            expected_length, actual_length
        ))
    }
}

impl Value {
    fn as_number(&self) -> Result<i32, &'static str> {
        match self {
            Value::Numeric(value) => Ok(*value),
            _ => Err("Invalid argument type"),
        }
    }
}

#[cfg(test)]
mod test;
