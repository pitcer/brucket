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

use crate::parser::{Expression, Function};

#[derive(Debug, PartialEq)]
pub enum Value {
    Unit,
    Numeric(i32),
    Textual(String),
}

impl Value {
    fn to_numeric(&self) -> Result<i32, String> {
        match self {
            Value::Numeric(value) => Ok(*value),
            _ => Err(String::from("Value is not a number")),
        }
    }

    fn to_textual(&self) -> Result<&String, String> {
        match self {
            Value::Textual(value) => Ok(value),
            _ => Err(String::from("Value is not a text")),
        }
    }
}

pub fn evaluate(expression: &Expression) -> Result<Value, String> {
    match expression {
        Expression::Constant(value) => Ok(Value::Numeric(*value as i32)),
        Expression::Symbol(value) => Ok(Value::Textual(value.clone())),
        Expression::Function(function) => match function {
            Function::Unit => Ok(Value::Unit),
            Function::Constant(name) => evaluate_constant_function(name),
            Function::NAry(name, arguments) => evaluate_n_ary_function(name, arguments),
        },
    }
}

fn evaluate_constant_function(name: &Expression) -> Result<Value, String> {
    let name = evaluate(name)?;
    let function = get_function(&name)?;
    Ok(function(Vec::new()))
}

fn evaluate_n_ary_function(name: &Expression, arguments: &[Expression]) -> Result<Value, String> {
    let name = evaluate(name)?;
    let function = get_function(&name)?;
    let evaluated_arguments = arguments
        .iter()
        .map(|argument| evaluate(argument).expect("Cannot evaluate function argument"))
        .collect();
    Ok(function(evaluated_arguments))
}

fn get_function(name_value: &Value) -> Result<fn(Vec<Value>) -> Value, String> {
    match name_value.to_textual()?.as_str() {
        "+" => {
            Ok(|arguments| calculate_numeric_function(arguments, |first, second| first + second))
        }
        "-" => {
            Ok(|arguments| calculate_numeric_function(arguments, |first, second| first - second))
        }
        "*" => {
            Ok(|arguments| calculate_numeric_function(arguments, |first, second| first * second))
        }
        "/" => {
            Ok(|arguments| calculate_numeric_function(arguments, |first, second| first / second))
        }
        "%" => {
            Ok(|arguments| calculate_numeric_function(arguments, |first, second| first % second))
        }
        "concatenate" => Ok(concatenate_function),
        _ => Err(format!("Cannot find function named {:?}", name_value)),
    }
}

fn calculate_numeric_function(arguments: Vec<Value>, function: fn(i32, i32) -> i32) -> Value {
    let first = arguments[0].to_numeric().expect("Type error");
    let second = arguments[1].to_numeric().expect("Type error");
    let result = function(first, second);
    Value::Numeric(result)
}

fn concatenate_function(arguments: Vec<Value>) -> Value {
    let mut result = String::new();
    for argument in arguments {
        match argument {
            Value::Textual(value) => result.push_str(value.as_str()),
            Value::Numeric(value) => result.push_str(value.to_string().as_str()),
            Value::Unit => (),
        }
    }
    Value::Textual(result)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_evaluated_constant_expression_is_numeric_value() -> Result<(), String> {
        let expected = Value::Numeric(42);
        let actual = evaluate(&Expression::Constant(42))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_evaluated_symbol_expression_is_textual_value() -> Result<(), String> {
        let expected = Value::Textual("foobar".to_string());
        let actual = evaluate(&Expression::Symbol("foobar".to_string()))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_evaluated_unit_function_expression_is_unit_value() -> Result<(), String> {
        let expected = Value::Unit;
        let actual = evaluate(&Expression::Function(Function::Unit))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_evaluated_addition_function_expression_is_numeric_value() -> Result<(), String> {
        let expected = Value::Numeric(42 + 24);
        let actual = evaluate(&Expression::Function(Function::NAry(
            Box::new(Expression::Symbol("+".to_string())),
            vec![Expression::Constant(42), Expression::Constant(24)],
        )))?;
        assert_eq!(expected, actual);
        Ok(())
    }
}
