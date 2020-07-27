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

type ExpressionResult = Result<Expression, String>;

pub struct Parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Constant(Constant),
    Symbol(String),
    Function(Box<Expression>, Vec<Expression>),
    Let(String, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Unit,
    Numeric(u32),
    Boolean(bool),
    String(String),
}

impl Parser {
    pub fn default() -> Parser {
        Parser {}
    }

    pub fn parse(&self, tokens: &[Token]) -> ExpressionResult {
        let mut iterator = tokens.iter();
        Parser::parse_first(&mut iterator)
    }

    fn parse_first(tokens: &mut Iter<Token>) -> ExpressionResult {
        let next = tokens.next();
        if next.is_none() {
            return Err("Empty tokens".to_string());
        }
        let token = next.unwrap();
        Parser::parse_first_token(token, tokens)
    }

    fn parse_first_token(token: &Token, tokens: &mut Iter<Token>) -> ExpressionResult {
        match token {
            Token::Parenthesis(parenthesis) => match parenthesis {
                Parenthesis::Open(_) => Parser::parse_section(tokens),
                Parenthesis::Close(_) => Err("Unexpected close parenthesis".to_string()),
            },
            Token::String(value) => Ok(Expression::Constant(Constant::String(value.clone()))),
            Token::Number(value) => Ok(Expression::Constant(Constant::Numeric(*value))),
            Token::Boolean(value) => Ok(Expression::Constant(Constant::Boolean(*value))),
            Token::Keyword(_keyword) => Err("Unexpected keyword".to_string()),
            Token::Symbol(symbol) => Ok(Expression::Symbol(symbol.clone())),
        }
    }

    fn parse_section(tokens: &mut Iter<Token>) -> ExpressionResult {
        let next = tokens.next();
        if next.is_none() {
            return Err("Empty tokens".to_string());
        }
        let token = next.unwrap();
        match token {
            Token::Parenthesis(parenthesis) => match parenthesis {
                Parenthesis::Open(_) => {
                    let function_name = Parser::parse_section(tokens)?;
                    Parser::parse_function(function_name, tokens)
                }
                Parenthesis::Close(_) => Ok(Expression::Constant(Constant::Unit)),
            },
            Token::Keyword(keyword) => match keyword {
                Keyword::Let => Parser::parse_let(tokens),
                Keyword::If => Parser::parse_if(tokens),
            },
            Token::Symbol(symbol) => {
                let function_name = Expression::Symbol(symbol.clone());
                Parser::parse_function(function_name, tokens)
            }
            _ => Err("Invalid token".to_string()),
        }
    }

    fn parse_function(name: Expression, tokens: &mut Iter<Token>) -> ExpressionResult {
        let mut arguments = Vec::new();
        let mut next = tokens.next();
        while next.is_some() {
            let current = next.unwrap();
            if Parser::is_close_parenthesis(current) {
                break;
            }
            let argument = Parser::parse_first_token(current, tokens)?;
            arguments.push(argument);
            next = tokens.next();
        }
        let name = Box::from(name);
        Ok(Expression::Function(name, arguments))
    }

    fn parse_let(tokens: &mut Iter<Token>) -> ExpressionResult {
        let name = Parser::parse_let_name(tokens)?;
        let value = Parser::parse_first(tokens)?;
        let then = Parser::parse_first(tokens)?;
        let next = tokens.next();
        if next.is_none() || !Parser::is_close_parenthesis(next.unwrap()) {
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

    fn parse_if(tokens: &mut Iter<Token>) -> ExpressionResult {
        let condition = Parser::parse_first(tokens)?;
        let if_true_then = Parser::parse_first(tokens)?;
        let if_false_then = Parser::parse_first(tokens)?;
        let next = tokens.next();
        if next.is_none() || !Parser::is_close_parenthesis(next.unwrap()) {
            Err("Invalid if expression".to_string())
        } else {
            Ok(Expression::If(
                Box::new(condition),
                Box::new(if_true_then),
                Box::new(if_false_then),
            ))
        }
    }

    fn is_close_parenthesis(token: &Token) -> bool {
        matches!(token, Token::Parenthesis(Parenthesis::Close(_)))
    }
}

#[cfg(test)]
mod test;
