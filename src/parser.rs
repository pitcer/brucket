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

use std::slice::Iter;

use crate::lexer::{Parenthesis, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Constant(u32),
    Symbol(String),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    Unit,
    Constant(Box<Expression>),
    NAry(Box<Expression>, Vec<Expression>),
}

pub fn parse(tokens: &[Token]) -> Result<Expression, String> {
    let mut iterator = tokens.iter();
    parse_iterator(&mut iterator)
}

fn parse_iterator(tokens_iterator: &mut Iter<Token>) -> Result<Expression, String> {
    let mut arguments: Vec<Expression> = Vec::new();
    let mut current;
    loop {
        let next = tokens_iterator.next();
        if next.is_none() {
            let first = arguments.remove_first().expect("Empty arguments");
            return Ok(first);
        }
        current = next.unwrap();
        match current {
            Token::Parenthesis(Parenthesis::Open(_)) => {
                arguments.push(parse_iterator(tokens_iterator)?)
            }
            Token::Parenthesis(Parenthesis::Close(_)) => {
                return Ok(Expression::Function(get_function(arguments)))
            }
            Token::Number(number) => arguments.push(Expression::Constant(*number)),
            Token::Symbol(symbol) => arguments.push(Expression::Symbol(symbol.clone())),
            _ => (),
        }
    }
}

trait RemoveFirst<T> {
    fn remove_first(&mut self) -> Option<T>;
}

impl<T> RemoveFirst<T> for Vec<T> {
    fn remove_first(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            let first = self.remove(0);
            Some(first)
        }
    }
}

fn get_function(mut arguments: Vec<Expression>) -> Function {
    let length = arguments.len();
    if length == 0 {
        return Function::Unit;
    }
    let name = arguments.remove(0);
    let name = Box::new(name);
    if length == 1 {
        Function::Constant(name)
    } else {
        Function::NAry(name, arguments)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parsed_number_token_is_constant_expression() -> Result<(), String> {
        let expected = Expression::Constant(42);
        let actual = parse(&[Token::Number(42)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_parsed_symbol_token_is_symbol_expression() -> Result<(), String> {
        let expected = Expression::Symbol("foobar".to_string());
        let actual = parse(&[Token::Symbol("foobar".to_string())])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_parsed_unit_function_tokens_are_function_expression() -> Result<(), String> {
        let expected = Expression::Function(Function::Unit);
        let actual = parse(&[
            Token::Parenthesis(Parenthesis::Open('(')),
            Token::Parenthesis(Parenthesis::Close(')')),
        ])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_parsed_constant_function_tokens_are_function_expression() -> Result<(), String> {
        let expected = Expression::Function(Function::Constant(Box::new(Expression::Symbol(
            "foobar".to_string(),
        ))));
        let actual = parse(&[
            Token::Parenthesis(Parenthesis::Open('(')),
            Token::Symbol("foobar".to_string()),
            Token::Parenthesis(Parenthesis::Close(')')),
        ])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_parsed_function_tokens_are_function_expression() -> Result<(), String> {
        let expected = Expression::Function(Function::NAry(
            Box::new(Expression::Symbol("foobar".to_string())),
            vec![Expression::Constant(42), Expression::Constant(24)],
        ));
        let actual = parse(&[
            Token::Parenthesis(Parenthesis::Open('(')),
            Token::Symbol("foobar".to_string()),
            Token::Number(42),
            Token::Number(24),
            Token::Parenthesis(Parenthesis::Close(')')),
        ])?;
        assert_eq!(expected, actual);
        Ok(())
    }
}
