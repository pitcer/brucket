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

pub fn add(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first + second))
}

pub fn subtract(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first - second))
}

pub fn multiply(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first * second))
}

pub fn divide(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first / second))
}

pub fn remainder(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Numeric(first % second))
}

pub fn is_equal(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    Ok(Value::Boolean(first == second))
}

pub fn is_greater(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Boolean(first > second))
}

pub fn is_greater_or_equal(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;

    Ok(Value::Boolean(first >= second))
}

pub fn is_less(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Boolean(first < second))
}

pub fn is_less_or_equal(arguments: Vec<Value>) -> ValueResult {
    let (first, second) = get_binary_function_arguments(arguments)?;
    let first = first.as_number()?;
    let second = second.as_number()?;
    Ok(Value::Boolean(first <= second))
}

pub mod pair {
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
        if let Value::Numeric(value) = self {
            Ok(*value)
        } else {
            Err("Invalid argument type")
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    type TestResult = Result<(), String>;

    #[test]
    fn test_add_with_two_arguments_returns_their_sum() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = add(vec![Value::Numeric(40), Value::Numeric(2)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_subtract_with_two_arguments_returns_their_difference() -> TestResult {
        let expected = Value::Numeric(40);
        let actual = subtract(vec![Value::Numeric(42), Value::Numeric(2)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_multiply_with_two_arguments_returns_their_product() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = multiply(vec![Value::Numeric(21), Value::Numeric(2)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_divide_with_two_arguments_returns_their_quotient() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = divide(vec![Value::Numeric(84), Value::Numeric(2)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_remainder_with_two_arguments_returns_their_remainder() -> TestResult {
        let expected = Value::Numeric(3);
        let actual = remainder(vec![Value::Numeric(45), Value::Numeric(42)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_is_equal() -> TestResult {
        assert_eq!(
            Value::Boolean(false),
            is_equal(vec![Value::Numeric(42), Value::Numeric(24)])?
        );
        assert_eq!(
            Value::Boolean(true),
            is_equal(vec![Value::Numeric(42), Value::Numeric(42)])?
        );
        Ok(())
    }

    #[test]
    fn test_is_greater() -> TestResult {
        assert_eq!(
            Value::Boolean(false),
            is_greater(vec![Value::Numeric(24), Value::Numeric(42)])?
        );
        assert_eq!(
            Value::Boolean(true),
            is_greater(vec![Value::Numeric(42), Value::Numeric(24)])?
        );
        Ok(())
    }

    #[test]
    fn test_is_greater_or_equal() -> TestResult {
        assert_eq!(
            Value::Boolean(false),
            is_greater_or_equal(vec![Value::Numeric(24), Value::Numeric(42)])?
        );
        assert_eq!(
            Value::Boolean(true),
            is_greater_or_equal(vec![Value::Numeric(42), Value::Numeric(24)])?
        );
        assert_eq!(
            Value::Boolean(true),
            is_greater_or_equal(vec![Value::Numeric(42), Value::Numeric(42)])?
        );
        Ok(())
    }

    #[test]
    fn test_is_less() -> TestResult {
        assert_eq!(
            Value::Boolean(false),
            is_less(vec![Value::Numeric(42), Value::Numeric(24)])?
        );
        assert_eq!(
            Value::Boolean(true),
            is_less(vec![Value::Numeric(24), Value::Numeric(42)])?
        );
        Ok(())
    }

    #[test]
    fn test_is_less_or_equal() -> TestResult {
        assert_eq!(
            Value::Boolean(false),
            is_less_or_equal(vec![Value::Numeric(42), Value::Numeric(24)])?
        );
        assert_eq!(
            Value::Boolean(true),
            is_less_or_equal(vec![Value::Numeric(24), Value::Numeric(42)])?
        );
        assert_eq!(
            Value::Boolean(true),
            is_less_or_equal(vec![Value::Numeric(42), Value::Numeric(42)])?
        );
        Ok(())
    }

    #[test]
    fn test_pair_new() -> TestResult {
        assert_eq!(
            Value::Pair(Box::new(Value::Numeric(42)), Box::new(Value::Numeric(24))),
            pair::new(vec![Value::Numeric(42), Value::Numeric(24)])?
        );
        Ok(())
    }

    #[test]
    fn test_pair_first() -> TestResult {
        assert_eq!(
            Value::Numeric(42),
            pair::first(vec![Value::Pair(
                Box::new(Value::Numeric(42)),
                Box::new(Value::Numeric(24))
            )])?
        );
        Ok(())
    }

    #[test]
    fn test_pair_second() -> TestResult {
        assert_eq!(
            Value::Numeric(24),
            pair::second(vec![Value::Pair(
                Box::new(Value::Numeric(42)),
                Box::new(Value::Numeric(24))
            )])?
        );
        Ok(())
    }
}
