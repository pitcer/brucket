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
    Identifier(String),
    Call(Call),
    Let(String, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Lambda(Lambda),
    Module(String, Vec<Expression>),
    Identified(String, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Unit,
    Numeric(u32),
    Boolean(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Call {
    Empty(Box<Expression>),
    Unary(Box<Expression>, Box<Expression>),
    Internal(String, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lambda {
    Empty(Box<Expression>),
    Parametrized(String, Box<Expression>),
}

impl Parser {
    pub fn default() -> Self {
        Self {}
    }

    pub fn parse(&self, tokens: &[Token]) -> ExpressionResult {
        let mut iterator = tokens.iter();
        Self::parse_first(&mut iterator)
    }

    fn parse_first(tokens: &mut Iter<Token>) -> ExpressionResult {
        let next = tokens.next();
        if next.is_none() {
            return Err("Empty tokens".to_string());
        }
        let token = next.unwrap();
        Self::parse_first_token(token, tokens)
    }

    fn parse_first_token(token: &Token, tokens: &mut Iter<Token>) -> ExpressionResult {
        match token {
            Token::Parenthesis(parenthesis) => match parenthesis {
                Parenthesis::Open(_) => Self::parse_section(tokens),
                Parenthesis::Close(_) => Err("Unexpected close parenthesis".to_string()),
                Parenthesis::Parameters => Err("Unexpected parameters parenthesis".to_string()),
            },
            Token::String(value) => Ok(Expression::Constant(Constant::String(value.clone()))),
            Token::Number(value) => Ok(Expression::Constant(Constant::Numeric(*value))),
            Token::Boolean(value) => Ok(Expression::Constant(Constant::Boolean(*value))),
            Token::Keyword(_keyword) => Err("Unexpected keyword".to_string()),
            Token::Symbol(symbol) => Ok(Expression::Identifier(symbol.clone())),
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
                    let identifier = Self::parse_section(tokens)?;
                    Self::parse_call(identifier, tokens)
                }
                Parenthesis::Close(_) => Ok(Expression::Constant(Constant::Unit)),
                Parenthesis::Parameters => Err("Unexpected parameters parenthesis".to_string()),
            },
            Token::Keyword(keyword) => match keyword {
                Keyword::Let => Self::parse_let(tokens),
                Keyword::If => Self::parse_if(tokens),
                Keyword::Lambda => Self::parse_lambda(tokens),
                Keyword::Internal => Self::parse_internal(tokens),
                Keyword::Module => Self::parse_module(tokens),
                Keyword::Function => Self::parse_function(tokens),
                Keyword::Constant => Self::parse_constant(tokens),
            },
            Token::Symbol(symbol) => {
                let identifier = Expression::Identifier(symbol.clone());
                Self::parse_call(identifier, tokens)
            }
            _ => Err("Invalid token".to_string()),
        }
    }

    fn parse_call(identifier: Expression, tokens: &mut Iter<Token>) -> ExpressionResult {
        let identifier = Box::from(identifier);
        let arguments = Self::parse_arguments(tokens)?;
        Self::expand_call(identifier, arguments)
    }

    fn expand_call(identifier: Box<Expression>, arguments: Vec<Expression>) -> ExpressionResult {
        let mut iterator = arguments.into_iter();
        let argument = iterator.next();
        if argument.is_none() {
            return Ok(Expression::Call(Call::Empty(identifier)));
        }
        let argument = argument.unwrap();
        let mut call = Expression::Call(Call::Unary(identifier, Box::from(argument)));
        for argument in iterator {
            call = Expression::Call(Call::Unary(Box::from(call), Box::from(argument)))
        }
        Ok(call)
    }

    fn parse_let(tokens: &mut Iter<Token>) -> ExpressionResult {
        let name = Self::parse_identifier(tokens)?;
        let value = Self::parse_first(tokens)?;
        let then = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err("Invalid let expression".to_string());
        }
        Ok(Expression::Let(name, Box::new(value), Box::new(then)))
    }

    fn parse_if(tokens: &mut Iter<Token>) -> ExpressionResult {
        let condition = Self::parse_first(tokens)?;
        let if_true_then = Self::parse_first(tokens)?;
        let if_false_then = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err("Invalid if expression".to_string());
        }
        Ok(Expression::If(
            Box::new(condition),
            Box::new(if_true_then),
            Box::new(if_false_then),
        ))
    }

    fn parse_function(tokens: &mut Iter<Token>) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let lambda = Self::parse_lambda(tokens)?;
        Ok(Expression::Identified(identifier, Box::from(lambda)))
    }

    fn parse_lambda(tokens: &mut Iter<Token>) -> ExpressionResult {
        let parameters = Self::parse_parameters(tokens)?;
        let body = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err("Invalid lambda expression".to_string());
        }
        let body = Box::new(body);
        Self::expand_lambda(parameters, body)
    }

    fn expand_lambda(parameters: Vec<String>, body: Box<Expression>) -> ExpressionResult {
        let mut iterator = parameters.iter().rev();
        let parameter = iterator.next();
        if parameter.is_none() {
            return Ok(Expression::Lambda(Lambda::Empty(body)));
        }
        let parameter = parameter.unwrap();
        let mut lambda = Expression::Lambda(Lambda::Parametrized(parameter.clone(), body));
        for parameter in iterator {
            lambda = Expression::Lambda(Lambda::Parametrized(parameter.clone(), Box::new(lambda)));
        }
        Ok(lambda)
    }

    fn parse_internal(tokens: &mut Iter<Token>) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let arguments = Self::parse_arguments(tokens)?;
        Ok(Expression::Call(Call::Internal(identifier, arguments)))
    }

    fn parse_module(tokens: &mut Iter<Token>) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let members = Self::parse_arguments(tokens)?;
        Ok(Expression::Module(identifier, members))
    }

    fn parse_constant(tokens: &mut Iter<Token>) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let value = Self::parse_first(tokens)?;
        Ok(Expression::Identified(identifier, Box::from(value)))
    }

    fn parse_identifier(tokens: &mut Iter<Token>) -> Result<String, String> {
        let identifier = tokens.next();
        if identifier.is_none() {
            return Err("Missing name token".to_string());
        }
        let identifier = identifier.unwrap();
        identifier.as_symbol()
    }

    fn parse_arguments(tokens: &mut Iter<Token>) -> Result<Vec<Expression>, String> {
        let mut arguments = Vec::new();
        let mut next = tokens.next();
        while next.is_some() {
            let current = next.unwrap();
            if Self::is_close_parenthesis(current) {
                break;
            }
            let argument = Self::parse_first_token(current, tokens)?;
            arguments.push(argument);
            next = tokens.next();
        }
        Ok(arguments)
    }

    fn parse_parameters(tokens: &mut Iter<Token>) -> Result<Vec<String>, String> {
        let mut parameters = Vec::new();
        if !Self::is_parameters_section(tokens) {
            return Err("Missing parameters section".to_string());
        }
        let mut next = tokens.next();
        while next.is_some() {
            let current = next.unwrap();
            if Self::is_parameters_parenthesis(current) {
                break;
            }
            let parameter = current.as_symbol()?;
            parameters.push(parameter);
            next = tokens.next();
        }
        Ok(parameters)
    }

    fn is_section_closed(tokens: &mut Iter<Token>) -> bool {
        let next = tokens.next();
        next.is_some() && Self::is_close_parenthesis(next.unwrap())
    }

    fn is_close_parenthesis(token: &Token) -> bool {
        matches!(token, Token::Parenthesis(Parenthesis::Close(_)))
    }

    fn is_parameters_section(tokens: &mut Iter<Token>) -> bool {
        let next = tokens.next();
        next.is_some() && Self::is_parameters_parenthesis(next.unwrap())
    }

    fn is_parameters_parenthesis(token: &Token) -> bool {
        matches!(token, Token::Parenthesis(Parenthesis::Parameters))
    }
}

impl Token {
    fn as_symbol(&self) -> Result<String, String> {
        if let Token::Symbol(symbol) = self {
            Ok(symbol.clone())
        } else {
            Err("Token is not a symbol".to_string())
        }
    }
}

#[cfg(test)]
mod test;
