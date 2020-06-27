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

use std::io::{Read, Stdin};
use std::io;
use std::io::stdin;
use std::iter::Peekable;
use std::slice::Iter;
use std::str::Chars;

fn main() {
    let mut input = stdin();
    let input_data = read(&mut input).expect("Cannot read syntax from stdin");
    let tokens = lex(&input_data).expect("Cannot lex syntax");
    println!("Tokens: {:?}", tokens);
    let expression = parse(&tokens).expect("Cannot parse tokens");
    println!("Expression: {:?}", expression);
    let result = evaluate(&expression).expect("Cannot evaluate expression");
    println!("Result: {:?}", result);
}

fn read(input: &mut Stdin) -> io::Result<String> {
    let mut result = String::new();
    input.read_to_string(&mut result)?;
    Ok(result)
}

#[derive(Debug)]
enum Token {
    Parenthesis(char),
    Number(u32),
    Symbol(String)
}

fn lex(syntax: &String) -> Result<Vec<Token>, String> {
    let mut result = Vec::new();
    let mut chars = syntax.chars().peekable();
    let mut current;
    loop {
        let next = chars.next();
        if next.is_none() {
            return Ok(result);
        }
        current = next.unwrap();
        match current {
            '(' | ')' => result.push(Token::Parenthesis(current)),
            '0'..='9' => result.push(Token::Number(lex_number(&mut chars, current)?)),
            'A'..='Z' | 'a'..='z' | '_' | '+' | '-' | '*' | '/' | '%' => result.push(Token::Symbol(lex_symbol(&mut chars, current)?)),
            _ => ()
        }
    }
}

fn lex_number(chars: &mut Peekable<Chars>, last: char) -> Result<u32, String> {
    let mut result = last.to_digit(10).expect("Invalid digit");
    let mut current;
    loop {
        current = chars.peek().expect("lex_number: No chars left");
        match current {
            '0'..='9' => result = result * 10 + current.to_digit(10).expect("Invalid digit"),
            ' ' | ')' => return Ok(result),
            _ => ()
        }
        chars.next();
    }
}

fn lex_symbol(chars: &mut Peekable<Chars>, last: char) -> Result<String, String> {
    let mut result = String::new();
    result.push(last);
    let mut current;
    loop {
        current = chars.peek().expect("lex_symbol: No chars left");
        match current {
            ' ' | ')' => return Ok(result),
            _ => result.push(*current),
        }
        chars.next();
    }
}

#[derive(Debug, Clone)]
enum Expression {
    Constant(u32),
    Symbol(String),
    Function(Box<Expression>, Vec<Expression>)
}

fn parse(tokens: &Vec<Token>) -> Result<Expression, String> {
    let mut iterator = tokens.iter();
    parse_iterator(&mut iterator)
}

fn parse_iterator(tokens_iterator: &mut Iter<Token>) -> Result<Expression, String> {
    let mut function_name: Option<Box<Expression>> = None;
    let mut arguments: Vec<Expression> = Vec::new();
    let mut current;
    loop {
        let next = tokens_iterator.next();
        if next.is_none() {
            println!("{:?}", arguments);
            return Ok(arguments[0].clone());
        }
        current = next.unwrap();
        match current {
            Token::Parenthesis('(') => arguments.push(parse_iterator(tokens_iterator)?),
            Token::Parenthesis(')') => return Ok(Expression::Function(function_name.expect("No function name"), arguments)),
            Token::Number(number) => arguments.push(Expression::Constant(*number)),
            Token::Symbol(symbol) => {
                if function_name.is_none() {
                    function_name = Some(Box::new(Expression::Symbol(symbol.clone())))
                } else {
                    arguments.push(Expression::Symbol(symbol.clone()));
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug)]
enum Value {
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

fn evaluate(expression: &Expression) -> Result<Value, String> {
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
