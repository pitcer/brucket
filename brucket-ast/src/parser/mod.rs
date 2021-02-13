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

use std::borrow::Cow;
use std::collections::HashSet;
use std::iter::Peekable;
use std::option::Option::Some;
use std::vec::IntoIter;

use expression::Expression;

use crate::ast::{
    Application, ApplicationStrategy, Arity, Boolean, ComplexPath, Constant, ConstantValue,
    Function, If, InternalFunction, Lambda, LambdaType, Let, Module, Number, Parameter, Path, Type,
    Visibility,
};
use crate::token::{
    Boolean as BooleanToken, Keyword, Modifier, Number as NumberToken, Operator, Parenthesis,
    PrimitiveType, Token,
};

pub mod expression;
#[cfg(test)]
mod test;

type ExpressionError = Cow<'static, str>;
type ExpressionResult = Result<Expression, ExpressionError>;

pub type ParseResult<T> = Result<T, ParseError>;
pub type ParseError = Cow<'static, str>;

pub trait Parse<T> {
    fn parse(self) -> ParseResult<T>;
}

pub struct Parser;

impl Token {
    fn into_symbol(self) -> Result<String, ExpressionError> {
        match self {
            Token::Symbol(symbol) => Ok(symbol),
            _ => Err(format!("Token is not a symbol: {:?}", self).into()),
        }
    }

    fn is_open_parenthesis(&self) -> bool {
        matches!(self, Token::Parenthesis(Parenthesis::Open(_)))
    }

    fn is_close_parenthesis(&self) -> bool {
        matches!(self, Token::Parenthesis(Parenthesis::Close(_)))
    }
}

type Tokens = Peekable<IntoIter<Token>>;

impl Default for Parser {
    fn default() -> Self {
        Self {}
    }
}

impl Parse<Number> for NumberToken {
    fn parse(self) -> ParseResult<Number> {
        match self {
            NumberToken::Integer(value) => Ok(Number::Integer(value)),
            NumberToken::FloatingPoint(value) => Ok(Number::FloatingPoint(value)),
        }
    }
}

impl Parse<Boolean> for BooleanToken {
    fn parse(self) -> ParseResult<Boolean> {
        match self {
            BooleanToken::True => Ok(Boolean::True),
            BooleanToken::False => Ok(Boolean::False),
        }
    }
}

impl Parser {
    pub fn parse(&self, tokens: Vec<Token>) -> ExpressionResult {
        let mut iterator = tokens.into_iter().peekable();
        Self::parse_first(&mut iterator)
    }

    fn parse_first(tokens: &mut Tokens) -> ExpressionResult {
        match tokens.next() {
            Some(token) => Self::parse_first_token(token, tokens),
            None => Err("Empty tokens".into()),
        }
    }

    fn parse_first_token(token: Token, tokens: &mut Tokens) -> ExpressionResult {
        match token {
            Token::Parenthesis(parenthesis) => match parenthesis {
                Parenthesis::Open(_) => Self::parse_section(tokens),
                Parenthesis::Close(_) => Err("Unexpected close parenthesis".into()),
            },
            Token::Operator(operator) => match operator {
                Operator::Variadic => Err("Unexpected variadic operator".into()),
                Operator::Path => Err("Unexpected path operator".into()),
                Operator::Type => Err("Unexpected type operator".into()),
                Operator::SkinnyArrowRight => Err("Unexpected '->'".into()),
                Operator::ThickArrowRight => Err("Unexpected '=>'".into()),
            },
            Token::Null => Ok(Expression::ConstantValue(ConstantValue::Null)),
            Token::String(value) => Ok(Expression::ConstantValue(ConstantValue::String(value))),
            Token::Number(value) => Ok(Expression::ConstantValue(ConstantValue::Numeric(
                value.parse()?,
            ))),
            Token::Boolean(value) => Ok(Expression::ConstantValue(ConstantValue::Boolean(
                value.parse()?,
            ))),
            Token::Keyword(keyword) => Err(format!("Unexpected token: {:?}", keyword).into()),
            Token::Symbol(symbol) => Ok(Expression::Identifier(Self::parse_path_symbol(
                symbol, tokens,
            )?)),
            Token::Modifier(modifier) => {
                Err(format!("Unexpected token: {:?}", modifier).into())
            }
            Token::PrimitiveType(type_token) => {
                Err(format!("Unexpected token: {:?}", type_token).into())
            }
        }
    }

    fn parse_section(tokens: &mut Tokens) -> ExpressionResult {
        match tokens.next() {
            Some(token) => Self::parse_section_token(token, tokens),
            None => Err("Empty tokens".into()),
        }
    }

    fn parse_section_token(token: Token, tokens: &mut Tokens) -> ExpressionResult {
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
                Keyword::Module => Self::parse_module(Vec::new(), tokens),
                Keyword::Function => Self::parse_function(Vec::new(), tokens),
                Keyword::Constant => Self::parse_constant(Vec::new(), tokens),
            },
            Token::Modifier(modifier) => Self::parse_modifiers(modifier, tokens),
            Token::Symbol(symbol) => {
                let path = Self::parse_path_symbol(symbol, tokens)?;
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
        let next = tokens.peek();
        if let Some(token) = next {
            let value_type = match token {
                Token::Operator(Operator::Type) => {
                    tokens.next();
                    Self::parse_type(tokens)?
                }
                _ => Type::Any,
            };
            let value = Self::parse_first(tokens)?;
            let then = Self::parse_first(tokens)?;
            if !Self::is_section_closed(tokens) {
                return Err(Cow::from("Invalid let expression"));
            }
            Ok(Expression::Let(Let::new(
                name,
                value_type,
                Box::new(value),
                Box::new(then),
            )))
        } else {
            Err("Invalid let expression: end of tokens".into())
        }
    }

    fn parse_if(tokens: &mut Tokens) -> ExpressionResult {
        let condition = Self::parse_first(tokens)?;
        let if_true_then = Self::parse_first(tokens)?;
        let if_false_then = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid if expression"));
        }
        Ok(Expression::If(If::new(
            Box::new(condition),
            Box::new(if_true_then),
            Box::new(if_false_then),
        )))
    }

    fn parse_modifiers(first_modifier: Modifier, tokens: &mut Tokens) -> ExpressionResult {
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
        current_token: Token,
        modifiers: Vec<Modifier>,
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

    fn parse_function(modifiers: Vec<Modifier>, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let mut visibility = Visibility::Private;
        let mut application_strategy = ApplicationStrategy::Eager;
        let mut internal = false;
        for modifier in modifiers {
            match modifier {
                Modifier::Public => visibility = Visibility::Public,
                Modifier::Private => visibility = Visibility::Private,
                Modifier::Lazy => application_strategy = ApplicationStrategy::Lazy,
                Modifier::Internal => internal = true,
                Modifier::Static => {
                    return Err("Static is an invalid modifier for a function".into())
                }
            }
        }
        if internal {
            Self::parse_internal_function(tokens, identifier, visibility, application_strategy)
        } else {
            Self::parse_lambda_function(tokens, identifier, visibility, application_strategy)
        }
    }

    fn parse_lambda_function(
        tokens: &mut Tokens,
        identifier: String,
        visibility: Visibility,
        application_strategy: ApplicationStrategy,
    ) -> ExpressionResult {
        let lambda = Self::parse_lambda(tokens)?;
        Ok(Expression::Function(Function::new(
            visibility,
            application_strategy,
            identifier,
            lambda,
        )))
    }

    fn parse_internal_function(
        tokens: &mut Tokens,
        identifier: String,
        visibility: Visibility,
        application_strategy: ApplicationStrategy,
    ) -> ExpressionResult {
        let parameters = Self::parse_parameters(tokens)?;
        let return_type = Self::parse_return_type(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid internal function expression"));
        }
        Ok(Expression::InternalFunction(InternalFunction::new(
            visibility,
            application_strategy,
            identifier,
            parameters,
            return_type,
        )))
    }

    fn parse_lambda(tokens: &mut Tokens) -> Result<Lambda<Expression>, ExpressionError> {
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
            Self::parse_type_token(token, tokens)
        } else {
            Err("End of tokens".into())
        }
    }

    fn parse_type_token(token: Token, tokens: &mut Tokens) -> Result<Type, ExpressionError> {
        match token {
            Token::PrimitiveType(primitive_type) => match primitive_type {
                PrimitiveType::Boolean => Ok(Type::Boolean),
                PrimitiveType::Integer => Ok(Type::Integer),
                PrimitiveType::String => Ok(Type::String),
                PrimitiveType::Any => Ok(Type::Any),
                PrimitiveType::Float => Ok(Type::Float),
                PrimitiveType::Unit => Ok(Type::Unit),
            },
            Token::Symbol(symbol) => Ok(Type::Symbol(symbol)),
            Token::Parenthesis(Parenthesis::Open('(')) => {
                Self::parse_lambda_type(tokens).map(Type::Lambda)
            }
            _ => Err(Cow::from("Invalid type token")),
        }
    }

    fn parse_lambda_type(tokens: &mut Tokens) -> Result<LambdaType, ExpressionError> {
        let mut parameters_types = Vec::new();
        while let Some(token) = tokens.next() {
            if token == Token::Operator(Operator::SkinnyArrowRight) {
                break;
            }
            let parameter_type = Self::parse_type_token(token, tokens)?;
            parameters_types.push(parameter_type);
        }
        let return_type = Self::parse_type(tokens)?.into();
        if let Some(Token::Parenthesis(Parenthesis::Close(')'))) = tokens.next() {
            Ok(LambdaType::new(parameters_types, return_type))
        } else {
            Err("Missing close parenthesis in lambda type".into())
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
            Expression::Let(Let {
                name: _,
                value_type: _,
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
                    for identifier in function.body.used_identifiers() {
                        identifiers.insert(identifier.clone());
                    }
                }
                for constant in module.constants() {
                    Self::insert_used_identifiers(&constant.value, identifiers);
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
            Expression::InternalFunction(_) => (),
            Expression::Constant(Constant {
                visibility: _,
                name: _,
                value,
            }) => Self::insert_used_identifiers(value, identifiers),
        };
    }

    fn parse_module(modifiers: Vec<Modifier>, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let mut functions = Vec::new();
        let mut internal_functions = Vec::new();
        let mut constants = Vec::new();
        while let Some(token) = tokens.next() {
            if token.is_close_parenthesis() {
                break;
            }
            let member = Self::parse_first_token(token, tokens)?;
            match member {
                Expression::Function(function) => functions.push(function),
                Expression::InternalFunction(function) => internal_functions.push(function),
                Expression::Constant(constant) => constants.push(constant),
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
        let module = Module::new(
            is_static,
            identifier,
            functions,
            internal_functions,
            constants,
        );
        Ok(Expression::Module(module))
    }

    fn parse_constant(modifiers: Vec<Modifier>, tokens: &mut Tokens) -> ExpressionResult {
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
                Token::Operator(Operator::Path) => {
                    if last_path_operator {
                        return Err(Cow::from("Unexpected path operator"));
                    } else {
                        last_path_operator = true;
                        tokens.next();
                    }
                }
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
        identifier.into_symbol()
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

    fn parse_parameter(token: Token, tokens: &mut Tokens) -> Result<Parameter, ExpressionError> {
        let name = token.into_symbol()?;
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
