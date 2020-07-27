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

use crate::lexer::{Keyword, Parenthesis, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Constant(u32),
    Boolean(bool),
    String(String),
    Symbol(String),
    Function(Function),
    Let(String, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    Unit,
    Constant(Box<Expression>),
    NAry(Box<Expression>, Vec<Expression>),
}

pub fn parse(tokens: &[Token]) -> Result<Expression, String> {
    let mut iterator = tokens.iter();
    parse_first(&mut iterator)
}

fn parse_first(tokens: &mut Iter<Token>) -> Result<Expression, String> {
    let next = tokens.next();
    if next.is_none() {
        return Err("Empty tokens".to_string());
    }
    let token = next.unwrap();
    parse_first_token(token, tokens)
}

fn parse_first_token(token: &Token, tokens: &mut Iter<Token>) -> Result<Expression, String> {
    match token {
        Token::Parenthesis(parenthesis) => match parenthesis {
            Parenthesis::Open(_) => parse_section(tokens),
            Parenthesis::Close(_) => Err("Unexpected close parenthesis".to_string()),
        },
        Token::String(string) => Ok(Expression::String(string.to_string())),
        Token::Number(number) => Ok(Expression::Constant(*number)),
        Token::Boolean(boolean) => Ok(Expression::Boolean(*boolean)),
        Token::Keyword(_keyword) => Err("Unexpected keyword".to_string()),
        Token::Symbol(symbol) => Ok(Expression::Symbol(symbol.clone())),
    }
}

fn parse_section(tokens: &mut Iter<Token>) -> Result<Expression, String> {
    let next = tokens.next();
    if next.is_none() {
        return Err("Empty tokens".to_string());
    }
    let token = next.unwrap();
    match token {
        Token::Parenthesis(parenthesis) => match parenthesis {
            Parenthesis::Open(_) => {
                let function_name = parse_section(tokens)?;
                parse_function(function_name, tokens)
            }
            Parenthesis::Close(_) => Ok(Expression::Function(Function::Unit)),
        },
        Token::Keyword(keyword) => match keyword {
            Keyword::Let => parse_let(tokens),
        },
        Token::Symbol(symbol) => {
            let function_name = Expression::Symbol(symbol.clone());
            parse_function(function_name, tokens)
        }
        _ => Err("Invalid token".to_string()),
    }
}

fn parse_function(name: Expression, tokens: &mut Iter<Token>) -> Result<Expression, String> {
    let mut arguments = Vec::new();
    let mut next = tokens.next();
    while next.is_some() {
        let current = next.unwrap();
        if is_close_parenthesis(current) {
            break;
        }
        let argument = parse_first_token(current, tokens)?;
        arguments.push(argument);
        next = tokens.next();
    }
    let name = Box::from(name);
    let function = if arguments.is_empty() {
        Function::Constant(name)
    } else {
        Function::NAry(name, arguments)
    };
    Ok(Expression::Function(function))
}

fn is_close_parenthesis(token: &Token) -> bool {
    matches!(token, Token::Parenthesis(Parenthesis::Close(_)))
}

fn parse_let(tokens: &mut Iter<Token>) -> Result<Expression, String> {
    let name = parse_let_name(tokens)?;
    let value = parse_first(tokens)?;
    let then = parse_first(tokens)?;
    let next = tokens.next();
    if next.is_none() || !is_close_parenthesis(next.unwrap()) {
        Err("Invalid let expression".to_string())
    } else {
        Ok(Expression::Let(name, Box::new(value), Box::new(then)))
    }
}

fn parse_let_name(tokens: &mut Iter<Token>) -> Result<String, String> {
    let name = tokens.next();
    if name.is_none() {
        return Err("Missing name token".to_string());
    }
    let name = name.unwrap();
    if let Token::Symbol(name) = name {
        Ok(name.clone())
    } else {
        Err("Name is not a symbol".to_string())
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
    fn test_parsed_boolean_token_is_boolean_expression() -> Result<(), String> {
        let expected = Expression::Boolean(true);
        let actual = parse(&[Token::Boolean(true)])?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_parsed_string_token_is_string_expression() -> Result<(), String> {
        let expected = Expression::String("foobar".to_string());
        let actual = parse(&[Token::String("foobar".to_string())])?;
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

    #[test]
    fn test_parsed_let_tokens_are_let_expression() -> Result<(), String> {
        let expected = Expression::Let(
            "x".to_string(),
            Box::new(Expression::Constant(42)),
            Box::new(Expression::Symbol("x".to_string())),
        );
        let actual = parse(&[
            Token::Parenthesis(Parenthesis::Open('(')),
            Token::Keyword(Keyword::Let),
            Token::Symbol("x".to_string()),
            Token::Number(42),
            Token::Symbol("x".to_string()),
            Token::Parenthesis(Parenthesis::Close(')')),
        ])?;
        assert_eq!(expected, actual);
        Ok(())
    }
}
