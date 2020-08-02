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
    if arguments.is_empty() {
        Err("Invalid number of arguments".to_string())
    } else {
        let mut result = 0;
        for argument in arguments {
            let argument = argument.as_number()?;
            result += argument;
        }
        Ok(Value::Numeric(result))
    }
}

pub fn subtract(arguments: Vec<Value>) -> ValueResult {
    if arguments.is_empty() {
        Err("Invalid number of arguments".to_string())
    } else if arguments.len() == 1 {
        let argument = &arguments[0];
        let argument = argument.as_number()?;
        Ok(Value::Numeric(-argument))
    } else {
        let (first, arguments) = arguments.split_first().unwrap();
        let first = first.as_number()?;
        let mut result = first;
        for argument in arguments {
            let argument = argument.as_number()?;
            result -= argument
        }
        Ok(Value::Numeric(result))
    }
}

pub fn multiply(arguments: Vec<Value>) -> ValueResult {
    if arguments.is_empty() {
        Err("Invalid number of arguments".to_string())
    } else {
        let mut result = 1;
        for argument in arguments {
            let argument = argument.as_number()?;
            result *= argument;
        }
        Ok(Value::Numeric(result))
    }
}

pub fn divide(arguments: Vec<Value>) -> ValueResult {
    if arguments.is_empty() {
        Err("Invalid number of arguments".to_string())
    } else if arguments.len() == 1 {
        let argument = &arguments[0];
        let argument = argument.as_number()?;
        Ok(Value::Numeric(1 / argument))
    } else {
        let (first, arguments) = arguments.split_first().unwrap();
        let first = first.as_number()?;
        let mut result = first;
        for argument in arguments {
            let argument = argument.as_number()?;
            result /= argument
        }
        Ok(Value::Numeric(result))
    }
}

pub fn remainder(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 2 {
        let first = &arguments[0];
        let second = &arguments[1];
        let first = first.as_number()?;
        let second = second.as_number()?;
        Ok(Value::Numeric(first % second))
    } else {
        Err("Invalid number of arguments".to_string())
    }
}

pub fn is_equal(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 2 {
        let first = &arguments[0];
        let second = &arguments[1];
        Ok(Value::Boolean(first.eq(second)))
    } else {
        Err("Invalid number of arguments".to_string())
    }
}

pub fn is_greater(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 2 {
        let first = &arguments[0];
        let second = &arguments[1];
        let first = first.as_number()?;
        let second = second.as_number()?;
        Ok(Value::Boolean(first > second))
    } else {
        Err("Invalid number of arguments".to_string())
    }
}

pub fn is_greater_or_equal(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 2 {
        let first = &arguments[0];
        let second = &arguments[1];
        let first = first.as_number()?;
        let second = second.as_number()?;
        Ok(Value::Boolean(first >= second))
    } else {
        Err("Invalid number of arguments".to_string())
    }
}

pub fn is_less(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 2 {
        let first = &arguments[0];
        let second = &arguments[1];
        let first = first.as_number()?;
        let second = second.as_number()?;
        Ok(Value::Boolean(first < second))
    } else {
        Err("Invalid number of arguments".to_string())
    }
}

pub fn is_less_or_equal(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 2 {
        let first = &arguments[0];
        let second = &arguments[1];
        let first = first.as_number()?;
        let second = second.as_number()?;
        Ok(Value::Boolean(first <= second))
    } else {
        Err("Invalid number of arguments".to_string())
    }
}

pub fn pair_new(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 2 {
        let mut iter = arguments.into_iter();
        let first = iter.next().unwrap();
        let second = iter.next().unwrap();
        Ok(Value::Pair(Box::new(first), Box::new(second)))
    } else {
        Err("Invalid number of arguments".to_string())
    }
}

pub fn pair_first(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 1 {
        let mut iter = arguments.into_iter();
        let first = iter.next().unwrap();
        if let Value::Pair(first, _) = first {
            Ok(*first)
        } else {
            Err("Invalid type of argument".to_string())
        }
    } else {
        Err("Invalid number of arguments".to_string())
    }
}

pub fn pair_second(arguments: Vec<Value>) -> ValueResult {
    if arguments.len() == 1 {
        let mut iter = arguments.into_iter();
        let first = iter.next().unwrap();
        if let Value::Pair(_, second) = first {
            Ok(*second)
        } else {
            Err("Invalid type of argument".to_string())
        }
    } else {
        Err("Invalid number of arguments".to_string())
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
    fn test_add_with_one_argument_returns_that_argument() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = add(vec![Value::Numeric(42)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_add_with_two_arguments_returns_their_sum() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = add(vec![Value::Numeric(40), Value::Numeric(2)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_add_with_many_arguments_returns_their_sum() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = add(vec![
            Value::Numeric(18),
            Value::Numeric(2),
            Value::Numeric(9),
            Value::Numeric(13),
        ])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_subtract_with_one_argument_returns_that_argument_with_opposite_sign() -> TestResult {
        let expected = Value::Numeric(-42);
        let actual = subtract(vec![Value::Numeric(42)])?;
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
    fn test_subtract_with_many_arguments_returns_their_difference() -> TestResult {
        let expected = Value::Numeric(10);
        let actual = subtract(vec![
            Value::Numeric(42),
            Value::Numeric(10),
            Value::Numeric(20),
            Value::Numeric(2),
        ])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_multiply_with_one_argument_returns_that_argument() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = multiply(vec![Value::Numeric(42)])?;
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
    fn test_multiply_with_many_arguments_returns_their_product() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = multiply(vec![
            Value::Numeric(7),
            Value::Numeric(3),
            Value::Numeric(2),
            Value::Numeric(1),
        ])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_divide_with_one_argument_returns_inverse_of_that_argument() -> TestResult {
        let expected = Value::Numeric(1 / 42);
        let actual = divide(vec![Value::Numeric(42)])?;
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
    fn test_divide_with_many_arguments_returns_their_quotient() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = divide(vec![
            Value::Numeric(42 * 2 * 3 * 5),
            Value::Numeric(2),
            Value::Numeric(3),
            Value::Numeric(5),
        ])?;
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
            pair_new(vec![Value::Numeric(42), Value::Numeric(24)])?
        );
        Ok(())
    }

    #[test]
    fn test_pair_first() -> TestResult {
        assert_eq!(
            Value::Numeric(42),
            pair_first(vec![Value::Pair(
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
            pair_second(vec![Value::Pair(
                Box::new(Value::Numeric(42)),
                Box::new(Value::Numeric(24))
            )])?
        );
        Ok(())
    }
}
