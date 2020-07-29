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
    And(Vec<Expression>),
    Or(Vec<Expression>),
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

impl Token {
    fn as_symbol(&self) -> Result<String, String> {
        match self {
            Token::Symbol(symbol) => Ok(symbol.clone()),
            _ => Err("Token is not a symbol".to_string()),
        }
    }

    fn is_close_parenthesis(&self) -> bool {
        matches!(self, Token::Parenthesis(Parenthesis::Close(_)))
    }

    fn is_parameters_parenthesis(&self) -> bool {
        matches!(self, Token::Parenthesis(Parenthesis::Parameters))
    }
}

type Tokens<'a> = Iter<'a, Token>;

impl Parser {
    pub fn default() -> Self {
        Self {}
    }

    pub fn parse(&self, tokens: &[Token]) -> ExpressionResult {
        let mut iterator = tokens.iter();
        Self::parse_first(&mut iterator)
    }

    fn parse_first(tokens: &mut Tokens) -> ExpressionResult {
        match tokens.next() {
            Some(token) => Self::parse_first_token(token, tokens),
            None => Err("Empty tokens".to_string()),
        }
    }

    fn parse_first_token(token: &Token, tokens: &mut Tokens) -> ExpressionResult {
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
        match tokens.next() {
            Some(token) => Self::parse_section_token(token, tokens),
            None => Err("Empty tokens".to_string()),
        }
    }

    fn parse_section_token(token: &Token, tokens: &mut Tokens) -> ExpressionResult {
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
                Keyword::And => Self::parse_and(tokens),
                Keyword::Or => Self::parse_or(tokens),
            },
            Token::Symbol(symbol) => {
                let identifier = Expression::Identifier(symbol.clone());
                Self::parse_call(identifier, tokens)
            }
            _ => Err("Invalid token".to_string()),
        }
    }

    fn parse_call(identifier: Expression, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Box::from(identifier);
        let arguments = Self::parse_arguments(tokens)?;
        Self::expand_call(identifier, arguments)
    }

    fn expand_call(identifier: Box<Expression>, arguments: Vec<Expression>) -> ExpressionResult {
        let mut arguments = arguments.into_iter();
        match arguments.next() {
            Some(argument) => {
                let mut call = Expression::Call(Call::Unary(identifier, Box::from(argument)));
                for argument in arguments {
                    call = Expression::Call(Call::Unary(Box::from(call), Box::from(argument)))
                }
                Ok(call)
            }
            None => Ok(Expression::Call(Call::Empty(identifier))),
        }
    }

    fn parse_let(tokens: &mut Tokens) -> ExpressionResult {
        let name = Self::parse_identifier(tokens)?;
        let value = Self::parse_first(tokens)?;
        let then = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err("Invalid let expression".to_string());
        }
        Ok(Expression::Let(name, Box::new(value), Box::new(then)))
    }

    fn parse_if(tokens: &mut Tokens) -> ExpressionResult {
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

    fn parse_function(tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let lambda = Self::parse_lambda(tokens)?;
        Ok(Expression::Identified(identifier, Box::from(lambda)))
    }

    fn parse_lambda(tokens: &mut Tokens) -> ExpressionResult {
        let parameters = Self::parse_parameters(tokens)?;
        let body = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err("Invalid lambda expression".to_string());
        }
        let body = Box::new(body);
        Self::expand_lambda(parameters, body)
    }

    fn expand_lambda(parameters: Vec<String>, body: Box<Expression>) -> ExpressionResult {
        let mut parameters = parameters.iter().rev();
        match parameters.next() {
            Some(parameter) => {
                let mut lambda = Expression::Lambda(Lambda::Parametrized(parameter.clone(), body));
                for parameter in parameters {
                    lambda = Expression::Lambda(Lambda::Parametrized(
                        parameter.clone(),
                        Box::new(lambda),
                    ));
                }
                Ok(lambda)
            }
            None => Ok(Expression::Lambda(Lambda::Empty(body))),
        }
    }

    fn parse_internal(tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let arguments = Self::parse_arguments(tokens)?;
        Ok(Expression::Call(Call::Internal(identifier, arguments)))
    }

    fn parse_module(tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let members = Self::parse_arguments(tokens)?;
        Ok(Expression::Module(identifier, members))
    }

    fn parse_constant(tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let value = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err("Missing parameters section".to_string());
        }
        Ok(Expression::Identified(identifier, Box::from(value)))
    }

    fn parse_and(tokens: &mut Tokens) -> ExpressionResult {
        let arguments = Self::parse_arguments(tokens)?;
        Ok(Expression::And(arguments))
    }

    fn parse_or(tokens: &mut Tokens) -> ExpressionResult {
        let arguments = Self::parse_arguments(tokens)?;
        Ok(Expression::Or(arguments))
    }

    fn parse_identifier(tokens: &mut Tokens) -> Result<String, String> {
        let identifier = tokens.next();
        if identifier.is_none() {
            return Err("Missing name token".to_string());
        }
        let identifier = identifier.unwrap();
        identifier.as_symbol()
    }

    fn parse_arguments(tokens: &mut Tokens) -> Result<Vec<Expression>, String> {
        let mut arguments = Vec::new();
        while let Some(token) = tokens.next() {
            if token.is_close_parenthesis() {
                break;
            }
            let argument = Self::parse_first_token(token, tokens)?;
            arguments.push(argument);
        }
        Ok(arguments)
    }

    fn parse_parameters(tokens: &mut Tokens) -> Result<Vec<String>, String> {
        let mut parameters = Vec::new();
        if !Self::is_parameters_section(tokens) {
            return Err("Missing parameters section".to_string());
        }
        for token in tokens {
            if token.is_parameters_parenthesis() {
                break;
            }
            let parameter = token.as_symbol()?;
            parameters.push(parameter);
        }
        Ok(parameters)
    }

    fn is_section_closed(tokens: &mut Tokens) -> bool {
        match tokens.next() {
            Some(token) => token.is_close_parenthesis(),
            None => false,
        }
    }

    fn is_parameters_section(tokens: &mut Tokens) -> bool {
        match tokens.next() {
            Some(token) => token.is_parameters_parenthesis(),
            None => false,
        }
    }
}

#[cfg(test)]
mod test;
