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

use crate::parser::Expression;

#[derive(Debug, PartialEq)]
pub enum Value {
    Numeric(i32),
    Textual(String)
}

impl Value {
    fn to_numeric(&self) -> Result<i32, String> {
        match self {
            Value::Numeric(value) => Ok(*value),
            _ => Err(String::from("Value is not a number"))
        }
    }

    fn to_textual(&self) -> Result<&String, String> {
        match self {
            Value::Textual(value) => Ok(value),
            _ => Err(String::from("Value is not a text"))
        }
    }
}

pub fn evaluate(expression: &Expression) -> Result<Value, String> {
    match expression {
        Expression::Constant(value) => Ok(Value::Numeric(*value as i32)),
        Expression::Symbol(value) => Ok(Value::Textual(value.clone())),
        Expression::Function(name, arguments) => {
            let evaluated_name = evaluate(name)?;
            let evaluated_arguments = arguments.iter()
                .map(|argument| evaluate(argument).expect("Cannot evaluate function argument"))
                .collect();
            let function = get_function(&evaluated_name)?;
            Ok(function(evaluated_arguments))
        },
    }
}

fn get_function(name_value: &Value) -> Result<fn(Vec<Value>) -> Value, String> {
    match name_value.to_textual()?.as_str() {
        "+" => Ok(|arguments| calculate_numeric_function(arguments, |first, second| first + second)),
        "-" => Ok(|arguments| calculate_numeric_function(arguments, |first, second| first - second)),
        "*" => Ok(|arguments| calculate_numeric_function(arguments, |first, second| first * second)),
        "/" => Ok(|arguments| calculate_numeric_function(arguments, |first, second| first / second)),
        "%" => Ok(|arguments| calculate_numeric_function(arguments, |first, second| first % second)),
        "concatenate" => Ok(concatenate_function),
        _ => Err(format!("Cannot find function named {:?}", name_value))
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
            Value::Numeric(value) => result.push_str(value.to_string().as_str())
        }
    }
    Value::Textual(result)
}
