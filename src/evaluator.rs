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
use std::collections::HashMap;

type ValueResult = Result<Value, String>;
type Environment = HashMap<String, Value>;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Numeric(i32),
    Textual(String),
    Boolean(bool),
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

pub fn evaluate(expression: &Expression) -> ValueResult {
    let mut environment = Environment::new();
    evaluate_environment(expression, &mut environment)
}

fn evaluate_environment(expression: &Expression, environment: &mut Environment) -> ValueResult {
    match expression {
        Expression::Constant(value) => Ok(Value::Numeric(*value as i32)),
        Expression::Boolean(value) => Ok(Value::Boolean(*value)),
        Expression::String(value) => Ok(Value::Textual(value.clone())),
        Expression::Symbol(value) => get_from_environment(environment, value),
        Expression::Function(function) => match function {
            Function::Unit => Ok(Value::Unit),
            Function::Constant(name) => evaluate_constant_function(name, environment),
            Function::NAry(name, arguments) => {
                evaluate_n_ary_function(name, arguments, environment)
            }
        },
        Expression::Let(name, value, then) => evaluate_let(name, value, then, environment),
    }
}

fn evaluate_let(
    name: &str,
    value: &Expression,
    then: &Expression,
    environment: &mut Environment,
) -> ValueResult {
    let value = evaluate_environment(value, environment)?;
    environment.insert(name.to_string(), value);
    let result = evaluate_environment(then, environment);
    environment.remove(name);
    result
}

fn get_from_environment(environment: &mut Environment, name: &str) -> ValueResult {
    let value = environment.get(name);
    if let Some(value) = value {
        return Ok(value.clone());
    }
    if is_builtin_function(name) {
        return Ok(Value::Textual(name.to_string()));
    }
    Err(format!("Undefined variable: {}", name))
}

fn is_builtin_function(name: &str) -> bool {
    match name {
        "+" | "-" | "*" | "/" | "%" | "concatenate" => true,
        _ => false,
    }
}

fn evaluate_constant_function(
    name: &Expression,
    environment: &mut Environment,
) -> Result<Value, String> {
    let name = evaluate_environment(name, environment)?;
    let function = get_function(&name)?;
    Ok(function(Vec::new()))
}

fn evaluate_n_ary_function(
    name: &Expression,
    arguments: &[Expression],
    environment: &mut Environment,
) -> ValueResult {
    let name = evaluate_environment(name, environment)?;
    let function = get_function(&name)?;
    let evaluated_arguments = arguments
        .iter()
        .map(|argument| {
            evaluate_environment(argument, environment).expect("Cannot evaluate function argument")
        })
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
            Value::Boolean(value) => result.push_str(value.to_string().as_str()),
            Value::Unit => (),
        }
    }
    Value::Textual(result)
}

#[cfg(test)]
mod test {
    use super::*;

    type TestResult = Result<(), String>;

    #[test]
    fn test_evaluated_constant_expression_is_numeric_value() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = evaluate(&Expression::Constant(42))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_evaluated_boolean_expression_is_boolean_value() -> TestResult {
        let expected = Value::Boolean(true);
        let actual = evaluate(&Expression::Boolean(true))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_evaluated_unit_function_expression_is_unit_value() -> TestResult {
        let expected = Value::Unit;
        let actual = evaluate(&Expression::Function(Function::Unit))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_evaluated_addition_function_expression_is_numeric_value() -> TestResult {
        let expected = Value::Numeric(42 + 24);
        let actual = evaluate(&Expression::Function(Function::NAry(
            Box::new(Expression::Symbol("+".to_string())),
            vec![Expression::Constant(42), Expression::Constant(24)],
        )))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_evaluated_let_expression_variable_has_correct_value() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = evaluate(&Expression::Let(
            "x".to_string(),
            Box::new(Expression::Constant(42)),
            Box::new(Expression::Symbol("x".to_string())),
        ))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    #[should_panic(expected = "Cannot evaluate function argument: \"Undefined variable: x\"")]
    fn test_environment_is_cleared() {
        let _result = evaluate(&Expression::Function(Function::NAry(
            Box::new(Expression::Symbol("+".to_string())),
            vec![
                Expression::Let(
                    "x".to_string(),
                    Box::new(Expression::Constant(42)),
                    Box::new(Expression::Symbol("x".to_string())),
                ),
                Expression::Symbol("x".to_string()),
            ],
        )));
    }

    #[test]
    fn test_first_variable_is_not_overwritten_by_second_with_different_name() -> TestResult {
        let expected = Value::Numeric(40);
        let actual = evaluate(&Expression::Let(
            "x".to_string(),
            Box::new(Expression::Constant(40)),
            Box::new(Expression::Let(
                "y".to_string(),
                Box::new(Expression::Constant(2)),
                Box::new(Expression::Symbol("x".to_string())),
            )),
        ))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_first_variable_is_overwritten_by_second_with_the_same_name() -> TestResult {
        let expected = Value::Numeric(2);
        let actual = evaluate(&Expression::Let(
            "x".to_string(),
            Box::new(Expression::Constant(40)),
            Box::new(Expression::Let(
                "x".to_string(),
                Box::new(Expression::Constant(2)),
                Box::new(Expression::Symbol("x".to_string())),
            )),
        ))?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_two_variables_can_be_used_in_function_evaluation() -> TestResult {
        let expected = Value::Numeric(42);
        let actual = evaluate(&Expression::Let(
            "x".to_string(),
            Box::new(Expression::Constant(40)),
            Box::new(Expression::Let(
                "y".to_string(),
                Box::new(Expression::Constant(2)),
                Box::new(Expression::Function(Function::NAry(
                    Box::new(Expression::Symbol("+".to_string())),
                    vec![
                        Expression::Symbol("x".to_string()),
                        Expression::Symbol("y".to_string()),
                    ],
                ))),
            )),
        ))?;
        assert_eq!(expected, actual);
        Ok(())
    }
}
