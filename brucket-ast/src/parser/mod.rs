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

use std::collections::HashSet;
use std::iter::Peekable;
use std::option::Option::Some;
use std::slice::Iter;

use crate::ast::{
    Application, ApplicationStrategy, Arity, Boolean, ComplexPath, Constant, ConstantValue,
    Expression, Function, If, InternalCall, Lambda, Let, Module, Number, Parameter, Path, Type,
    Visibility,
};

use crate::token::{
    Boolean as BooleanToken, Keyword, Modifier, Number as NumberToken, Operator, Parenthesis,
    PrimitiveType, Token,
};
use std::borrow::Cow;

#[cfg(test)]
mod test;

type ExpressionError = Cow<'static, str>;
type ExpressionResult = Result<Expression, ExpressionError>;

pub struct Parser;

impl Token {
    fn as_symbol(&self) -> Result<String, ExpressionError> {
        match self {
            Token::Symbol(symbol) => Ok(symbol.clone()),
            _ => Err(Cow::from(format!("Token is not a symbol: {:?}", self))),
        }
    }

    fn is_open_parenthesis(&self) -> bool {
        matches!(self, Token::Parenthesis(Parenthesis::Open(_)))
    }

    fn is_close_parenthesis(&self) -> bool {
        matches!(self, Token::Parenthesis(Parenthesis::Close(_)))
    }
}

type Tokens<'a> = Peekable<Iter<'a, Token>>;

impl Default for Parser {
    fn default() -> Self {
        Self {}
    }
}

impl NumberToken {
    fn to_expression(&self) -> Number {
        match self {
            NumberToken::Integer(value) => Number::Integer(value.to_string()),
            NumberToken::FloatingPoint(value) => Number::FloatingPoint(value.to_string()),
        }
    }
}

impl BooleanToken {
    fn to_expression(&self) -> Boolean {
        match self {
            BooleanToken::True => Boolean::True,
            BooleanToken::False => Boolean::False,
        }
    }
}

impl Parser {
    pub fn parse(&self, tokens: &[Token]) -> ExpressionResult {
        let mut iterator = tokens.iter().peekable();
        Self::parse_first(&mut iterator)
    }

    fn parse_first(tokens: &mut Tokens) -> ExpressionResult {
        match tokens.next() {
            Some(token) => Self::parse_first_token(token, tokens),
            None => Err(Cow::from("Empty tokens")),
        }
    }

    fn parse_first_token(token: &Token, tokens: &mut Tokens) -> ExpressionResult {
        match token {
            Token::Parenthesis(parenthesis) => match parenthesis {
                Parenthesis::Open(_) => Self::parse_section(tokens),
                Parenthesis::Close(_) => Err(Cow::from("Unexpected close parenthesis")),
            },
            Token::Operator(operator) => match operator {
                Operator::Variadic => Err(Cow::from("Unexpected variadic operator")),
                Operator::Path => Err(Cow::from("Unexpected path operator")),
                Operator::Type => Err(Cow::from("Unexpected type operator")),
                Operator::SkinnyArrowRight => Err(Cow::from("Unexpected '->'")),
                Operator::ThickArrowRight => Err(Cow::from("Unexpected '=>'")),
            },
            Token::Null => Ok(Expression::ConstantValue(ConstantValue::Null)),
            Token::String(value) => Ok(Expression::ConstantValue(ConstantValue::String(
                value.clone(),
            ))),
            Token::Number(value) => Ok(Expression::ConstantValue(ConstantValue::Numeric(
                value.to_expression(),
            ))),
            Token::Boolean(value) => Ok(Expression::ConstantValue(ConstantValue::Boolean(
                value.to_expression(),
            ))),
            Token::Keyword(keyword) => Err(Cow::from(format!("Unexpected token: {:?}", keyword))),
            Token::Symbol(symbol) => Ok(Expression::Identifier(Self::parse_path_symbol(
                symbol.clone(),
                tokens,
            )?)),
            Token::Modifier(modifier) => {
                Err(Cow::from(format!("Unexpected token: {:?}", modifier)))
            }
            Token::PrimitiveType(type_token) => {
                Err(Cow::from(format!("Unexpected token: {:?}", type_token)))
            }
        }
    }

    fn parse_section(tokens: &mut Tokens) -> ExpressionResult {
        match tokens.next() {
            Some(token) => Self::parse_section_token(token, tokens),
            None => Err(Cow::from("Empty tokens")),
        }
    }

    fn parse_section_token(token: &Token, tokens: &mut Tokens) -> ExpressionResult {
        match token {
            Token::Parenthesis(parenthesis) => match parenthesis {
                Parenthesis::Open(_) => {
                    let identifier = Self::parse_section(tokens)?;
                    Self::parse_application(identifier, tokens)
                }
                Parenthesis::Close(_) => Ok(Expression::ConstantValue(ConstantValue::Unit)),
            },
            Token::Keyword(keyword) => match keyword {
                Keyword::Let => Self::parse_let(tokens),
                Keyword::If => Self::parse_if(tokens),
                Keyword::Lambda => Self::parse_lambda(tokens).map(Expression::Lambda),
                Keyword::Internal => Self::parse_internal(tokens),
                Keyword::Module => Self::parse_module(Vec::new(), tokens),
                Keyword::Function => Self::parse_function(Vec::new(), tokens),
                Keyword::Constant => Self::parse_constant(Vec::new(), tokens),
            },
            Token::Modifier(modifier) => Self::parse_modifiers(modifier, tokens),
            Token::Symbol(symbol) => {
                let path = Self::parse_path_symbol(symbol.clone(), tokens)?;
                let identifier = Expression::Identifier(path);
                Self::parse_application(identifier, tokens)
            }
            Token::Operator(operator) => match operator {
                Operator::ThickArrowRight => Self::parse_lambda(tokens).map(Expression::Lambda),
                _ => Err(Cow::from(format!("Invalid operator: {:?}", operator))),
            },
            _ => Err(Cow::from(format!("Invalid token: {:?}", token))),
        }
    }

    fn parse_application(identifier: Expression, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Box::from(identifier);
        let arguments = Self::parse_arguments(tokens)?;
        Ok(Expression::Application(Application {
            identifier,
            arguments,
        }))
    }

    fn parse_let(tokens: &mut Tokens) -> ExpressionResult {
        let name = Self::parse_identifier(tokens)?;
        let value = Self::parse_first(tokens)?;
        let then = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid let expression"));
        }
        Ok(Expression::Let(Let::new(
            name,
            Box::new(value),
            Box::new(then),
        )))
    }

    fn parse_if(tokens: &mut Tokens) -> ExpressionResult {
        let condition = Self::parse_first(tokens)?;
        let if_true_then = Self::parse_first(tokens)?;
        let if_false_then = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid if expression"));
        }
        Ok(Expression::If(If {
            condition: Box::new(condition),
            if_true: Box::new(if_true_then),
            if_false: Box::new(if_false_then),
        }))
    }

    fn parse_modifiers(first_modifier: &Modifier, tokens: &mut Tokens) -> ExpressionResult {
        let mut modifiers = vec![first_modifier];
        while let Some(token) = tokens.next() {
            if let Token::Modifier(modifier) = token {
                modifiers.push(modifier);
            } else {
                return Self::parse_with_modifiers(token, modifiers, tokens);
            }
        }
        Err(Cow::from("Invalid token"))
    }

    fn parse_with_modifiers(
        current_token: &Token,
        modifiers: Vec<&Modifier>,
        tokens: &mut Tokens,
    ) -> ExpressionResult {
        match current_token {
            Token::Keyword(keyword) => match keyword {
                Keyword::Function => Self::parse_function(modifiers, tokens),
                Keyword::Constant => Self::parse_constant(modifiers, tokens),
                Keyword::Module => Self::parse_module(modifiers, tokens),
                _ => Err(Cow::from("Invalid token")),
            },
            _ => Err(Cow::from("Invalid token")),
        }
    }

    fn parse_function(modifiers: Vec<&Modifier>, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let lambda = Self::parse_lambda(tokens)?;
        let mut visibility = Visibility::Private;
        let mut application_strategy = ApplicationStrategy::Eager;
        for modifier in modifiers {
            match modifier {
                Modifier::Public => visibility = Visibility::Public,
                Modifier::Private => visibility = Visibility::Private,
                Modifier::Lazy => application_strategy = ApplicationStrategy::Lazy,
                Modifier::Static => {
                    return Err(Cow::from("Static is an invalid modifier for function"))
                }
            }
        }
        Ok(Expression::Function(Function {
            visibility,
            application_strategy,
            name: identifier,
            body: lambda,
        }))
    }

    fn parse_lambda(tokens: &mut Tokens) -> Result<Lambda, ExpressionError> {
        let parameters = Self::parse_parameters(tokens)?;
        let return_type = Self::parse_return_type(tokens)?;
        let body = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid lambda expression"));
        }
        let mut used_identifiers = HashSet::new();
        Self::insert_used_identifiers(&body, &mut used_identifiers);
        for parameter in &parameters {
            let name = parameter.name();
            used_identifiers.remove(name);
        }
        Ok(Lambda::new(
            parameters,
            return_type,
            Box::new(body),
            used_identifiers,
        ))
    }

    fn parse_return_type(tokens: &mut Tokens) -> Result<Type, ExpressionError> {
        let next = tokens.peek();
        if let Some(token) = next {
            match token {
                Token::Operator(Operator::SkinnyArrowRight) => {
                    tokens.next();
                    Self::parse_type(tokens)
                }
                _ => Ok(Type::Any),
            }
        } else {
            Err(Cow::from("An unexpected end of tokens"))
        }
    }

    fn parse_type(tokens: &mut Tokens) -> Result<Type, ExpressionError> {
        let next = tokens.next();
        if let Some(token) = next {
            match token {
                Token::PrimitiveType(primitive_type) => match primitive_type {
                    PrimitiveType::Boolean => Ok(Type::Boolean),
                    PrimitiveType::Integer => Ok(Type::Integer),
                    PrimitiveType::String => Ok(Type::String),
                    PrimitiveType::Any => Ok(Type::Any),
                },
                Token::Symbol(symbol) => Ok(Type::Symbol(symbol.clone())),
                _ => Err(Cow::from("Invalid type token")),
            }
        } else {
            Err(Cow::from("End of tokens"))
        }
    }

    fn insert_used_identifiers(expression: &Expression, identifiers: &mut HashSet<String>) {
        match expression {
            Expression::Identifier(identifier) => match identifier {
                Path::Simple(identifier) => {
                    identifiers.insert(identifier.clone());
                }
                Path::Complex(_) => (),
            },
            Expression::Application(Application {
                identifier,
                arguments,
            }) => {
                Self::insert_used_identifiers(identifier, identifiers);
                for argument in arguments {
                    Self::insert_used_identifiers(argument, identifiers);
                }
            }
            Expression::InternalCall(InternalCall {
                identifier: _,
                arguments,
            }) => {
                for argument in arguments {
                    Self::insert_used_identifiers(argument, identifiers);
                }
            }
            Expression::Let(Let {
                name: _,
                value,
                then,
            }) => {
                Self::insert_used_identifiers(value, identifiers);
                Self::insert_used_identifiers(then, identifiers);
            }
            Expression::If(If {
                condition,
                if_true,
                if_false,
            }) => {
                Self::insert_used_identifiers(condition, identifiers);
                Self::insert_used_identifiers(if_true, identifiers);
                Self::insert_used_identifiers(if_false, identifiers);
            }
            Expression::Lambda(lambda) => {
                for identifier in lambda.used_identifiers() {
                    identifiers.insert(identifier.clone());
                }
            }
            Expression::Module(module) => {
                for function in module.functions() {
                    Self::insert_used_identifiers(function, identifiers);
                }
                for constant in module.constants() {
                    Self::insert_used_identifiers(constant, identifiers);
                }
            }
            Expression::ConstantValue(_) => (),
            Expression::Function(Function {
                visibility: _,
                application_strategy: _,
                name: _,
                body,
            }) => {
                for identifier in body.used_identifiers() {
                    identifiers.insert(identifier.clone());
                }
            }
            Expression::Constant(Constant {
                visibility: _,
                name: _,
                value,
            }) => Self::insert_used_identifiers(value, identifiers),
        };
    }

    fn parse_internal(tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let arguments = Self::parse_arguments(tokens)?;
        Ok(Expression::InternalCall(InternalCall {
            identifier,
            arguments,
        }))
    }

    fn parse_module(modifiers: Vec<&Modifier>, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let mut functions = Vec::new();
        let mut constants = Vec::new();
        while let Some(token) = tokens.next() {
            if token.is_close_parenthesis() {
                break;
            }
            let member = Self::parse_first_token(token, tokens)?;
            match member {
                Expression::Function(_) => functions.push(member),
                Expression::Constant(_) => constants.push(member),
                _ => return Err(Cow::from(format!("Invalid module member: {:?}", member))),
            }
        }
        let mut is_static = false;
        for modifier in modifiers {
            match modifier {
                Modifier::Static => is_static = true,
                _ => return Err(Cow::from("Invalid module modifier")),
            }
        }
        let module = Module::new(is_static, identifier, functions, constants);
        Ok(Expression::Module(module))
    }

    fn parse_constant(modifiers: Vec<&Modifier>, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let value = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid constant expression"));
        }
        let mut visibility = Visibility::Private;
        for modifier in modifiers {
            match modifier {
                Modifier::Public => visibility = Visibility::Public,
                Modifier::Private => visibility = Visibility::Private,
                _ => return Err(Cow::from(format!("Invalid modifier: {:?}", modifier))),
            }
        }
        Ok(Expression::Constant(Constant::new(
            visibility,
            identifier,
            Box::from(value),
        )))
    }

    fn parse_path_symbol(symbol: String, tokens: &mut Tokens) -> Result<Path, ExpressionError> {
        let mut path = Vec::new();
        path.push(symbol);
        let mut last_path_operator = false;
        while let Some(token) = tokens.peek() {
            match token {
                Token::Operator(operator) => match operator {
                    Operator::Path => {
                        if last_path_operator {
                            return Err(Cow::from("Unexpected path operator"));
                        } else {
                            last_path_operator = true;
                            tokens.next();
                        }
                    }
                    _ => break,
                },
                Token::Symbol(symbol) => {
                    if last_path_operator {
                        last_path_operator = false;
                        path.push(symbol.clone());
                        tokens.next();
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        let length = path.len();
        let identifier = path.swap_remove(length - 1);
        if length == 1 {
            Ok(Path::Simple(identifier))
        } else {
            Ok(Path::Complex(ComplexPath::new(identifier, path)))
        }
    }

    fn parse_identifier(tokens: &mut Tokens) -> Result<String, ExpressionError> {
        let identifier = tokens.next();
        if identifier.is_none() {
            return Err(Cow::from("Missing name token"));
        }
        let identifier = identifier.unwrap();
        identifier.as_symbol()
    }

    fn parse_arguments(tokens: &mut Tokens) -> Result<Vec<Expression>, ExpressionError> {
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

    fn parse_parameters(tokens: &mut Tokens) -> Result<Vec<Parameter>, ExpressionError> {
        if !Self::is_section_opened(tokens) {
            return Err(Cow::from("Missing parameters section"));
        }
        let mut parameters = Vec::new();
        while let Some(token) = tokens.next() {
            if token.is_close_parenthesis() {
                break;
            }
            let parameter = Self::parse_parameter(token, tokens)?;
            parameters.push(parameter)
        }
        Ok(parameters)
    }

    fn parse_parameter(token: &Token, tokens: &mut Tokens) -> Result<Parameter, ExpressionError> {
        let name = token.as_symbol()?;
        let next = tokens.peek();
        if let Some(token) = next {
            match token {
                Token::Operator(operator) => match operator {
                    Operator::Variadic => {
                        tokens.next();
                        Ok(Parameter::new(name, Type::Any, Arity::Variadic))
                    }
                    Operator::Type => {
                        tokens.next();
                        let parameter_type = Self::parse_type(tokens)?;
                        let next = tokens.peek();
                        let arity = if let Some(next) = next {
                            if let Token::Operator(Operator::Variadic) = next {
                                tokens.next();
                                Arity::Variadic
                            } else {
                                Arity::Unary
                            }
                        } else {
                            return Err(Cow::from("End of tokens in parameters section"));
                        };
                        Ok(Parameter::new(name, parameter_type, arity))
                    }
                    _ => Err(Cow::from("Invalid operator")),
                },
                _ => Ok(Parameter::new(name, Type::Any, Arity::Unary)),
            }
        } else {
            Err(Cow::from("End of tokens in parameters section"))
        }
    }

    fn is_section_opened(tokens: &mut Tokens) -> bool {
        match tokens.next() {
            Some(token) => token.is_open_parenthesis(),
            None => false,
        }
    }

    fn is_section_closed(tokens: &mut Tokens) -> bool {
        match tokens.next() {
            Some(token) => token.is_close_parenthesis(),
            None => false,
        }
    }
}
