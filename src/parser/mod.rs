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
use std::slice::Iter;

use crate::lexer::{Keyword, Modifier, Operator, Parenthesis, Token};

type ExpressionResult = Result<Expression, String>;

pub struct Parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    ConstantValue(ConstantValue),
    Identifier(Path),
    Application(Box<Expression>, Vec<Expression>),
    InternalCall(String, Vec<Expression>),
    Let(String, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Lambda(Lambda),
    Module(Module),
    Function(Visibility, ApplicationStrategy, String, Lambda),
    Constant(Visibility, String, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    Simple(String),
    Complex(ComplexPath),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComplexPath {
    identifier: String,
    path: Vec<String>,
}

impl ComplexPath {
    pub fn new(identifier: String, path: Vec<String>) -> Self {
        Self { identifier, path }
    }

    pub fn identifier(&self) -> &String {
        &self.identifier
    }

    pub fn path(&self) -> &Vec<String> {
        &self.path
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    parameters: Vec<Parameter>,
    body: Box<Expression>,
    used_identifiers: HashSet<String>,
}

impl Lambda {
    pub fn new(
        parameters: Vec<Parameter>,
        body: Box<Expression>,
        used_identifiers: HashSet<String>,
    ) -> Self {
        Self {
            parameters,
            body,
            used_identifiers,
        }
    }

    pub fn parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    pub fn body(&self) -> &Expression {
        &self.body
    }

    pub fn used_identifiers(&self) -> &HashSet<String> {
        &self.used_identifiers
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    is_static: bool,
    identifier: String,
    functions: Vec<Expression>,
    constants: Vec<Expression>,
}

impl Module {
    pub fn new(
        is_static: bool,
        identifier: String,
        functions: Vec<Expression>,
        constants: Vec<Expression>,
    ) -> Self {
        Self {
            is_static,
            identifier,
            functions,
            constants,
        }
    }

    pub fn is_static(&self) -> bool {
        self.is_static
    }

    pub fn identifier(&self) -> &String {
        &self.identifier
    }

    pub fn functions(&self) -> &Vec<Expression> {
        &self.functions
    }

    pub fn constants(&self) -> &Vec<Expression> {
        &self.constants
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Unit,
    Null,
    Numeric(u32),
    Boolean(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    Unary(String),
    Variadic(String),
}

impl Parameter {
    pub fn name(&self) -> &String {
        match self {
            Parameter::Unary(name) => name,
            Parameter::Variadic(name) => name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public)
    }

    pub fn is_private(&self) -> bool {
        matches!(self, Visibility::Private)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ApplicationStrategy {
    Eager,
    Lazy,
}

impl ApplicationStrategy {
    pub fn is_eager(&self) -> bool {
        matches!(self, ApplicationStrategy::Eager)
    }

    pub fn is_lazy(&self) -> bool {
        matches!(self, ApplicationStrategy::Lazy)
    }
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

type Tokens<'a> = Peekable<Iter<'a, Token>>;

impl Default for Parser {
    fn default() -> Self {
        Self {}
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
            Token::Operator(operator) => match operator {
                Operator::Variadic => Err("Unexpected variadic operator".to_string()),
                Operator::Path => Err("Unexpected path operator".to_string()),
            },
            Token::Null => Ok(Expression::ConstantValue(ConstantValue::Null)),
            Token::String(value) => Ok(Expression::ConstantValue(ConstantValue::String(
                value.clone(),
            ))),
            Token::Number(value) => Ok(Expression::ConstantValue(ConstantValue::Numeric(*value))),
            Token::Boolean(value) => Ok(Expression::ConstantValue(ConstantValue::Boolean(*value))),
            Token::Keyword(keyword) => Err(format!("Unexpected token: {:?}", keyword)),
            Token::Symbol(symbol) => Ok(Expression::Identifier(Self::parse_path_symbol(
                symbol.clone(),
                tokens,
            )?)),
            Token::Modifier(modifier) => Err(format!("Unexpected token: {:?}", modifier)),
        }
    }

    fn parse_section(tokens: &mut Tokens) -> ExpressionResult {
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
                    Self::parse_application(identifier, tokens)
                }
                Parenthesis::Close(_) => Ok(Expression::ConstantValue(ConstantValue::Unit)),
                Parenthesis::Parameters => Err("Unexpected parameters parenthesis".to_string()),
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
            _ => Err("Invalid token".to_string()),
        }
    }

    fn parse_application(identifier: Expression, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Box::from(identifier);
        let arguments = Self::parse_arguments(tokens)?;
        Ok(Expression::Application(identifier, arguments))
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

    fn parse_modifiers(first_modifier: &Modifier, tokens: &mut Tokens) -> ExpressionResult {
        let mut modifiers = vec![first_modifier];
        while let Some(token) = tokens.next() {
            if let Token::Modifier(modifier) = token {
                modifiers.push(modifier);
            } else {
                return Self::parse_with_modifiers(token, modifiers, tokens);
            }
        }
        Err("Invalid token".to_string())
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
                _ => Err("Invalid token".to_string()),
            },
            _ => Err("Invalid token".to_string()),
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
                    return Err("Static is an invalid modifier for function".to_string())
                }
            }
        }
        Ok(Expression::Function(
            visibility,
            application_strategy,
            identifier,
            lambda,
        ))
    }

    fn parse_lambda(tokens: &mut Tokens) -> Result<Lambda, String> {
        let parameters = Self::parse_parameters(tokens)?;
        let body = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err("Invalid lambda expression".to_string());
        }
        let mut used_identifiers = HashSet::new();
        Self::insert_used_identifiers(&body, &mut used_identifiers);
        for parameter in &parameters {
            let name = parameter.name();
            used_identifiers.remove(name);
        }
        Ok(Lambda::new(parameters, Box::new(body), used_identifiers))
    }

    fn insert_used_identifiers(expression: &Expression, identifiers: &mut HashSet<String>) {
        match expression {
            Expression::Identifier(identifier) => match identifier {
                Path::Simple(identifier) => {
                    identifiers.insert(identifier.clone());
                }
                Path::Complex(_) => (),
            },
            Expression::Application(identifier, arguments) => {
                Self::insert_used_identifiers(identifier, identifiers);
                for argument in arguments {
                    Self::insert_used_identifiers(argument, identifiers);
                }
            }
            Expression::InternalCall(_, arguments) => {
                for argument in arguments {
                    Self::insert_used_identifiers(argument, identifiers);
                }
            }
            Expression::Let(_, value, body) => {
                Self::insert_used_identifiers(value, identifiers);
                Self::insert_used_identifiers(body, identifiers);
            }
            Expression::If(condition, if_true, if_false) => {
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
            Expression::Function(_, _, _, lambda) => {
                for identifier in lambda.used_identifiers() {
                    identifiers.insert(identifier.clone());
                }
            }
            Expression::Constant(_, _, value) => Self::insert_used_identifiers(value, identifiers),
        };
    }

    fn parse_internal(tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let arguments = Self::parse_arguments(tokens)?;
        Ok(Expression::InternalCall(identifier, arguments))
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
                Expression::Function(_, _, _, _) => functions.push(member),
                Expression::Constant(_, _, _) => constants.push(member),
                _ => return Err("Invalid module member".to_string()),
            }
        }
        let mut is_static = false;
        for modifier in modifiers {
            match modifier {
                Modifier::Static => is_static = true,
                _ => return Err("Invalid module modifier".to_string()),
            }
        }
        let module = Module::new(is_static, identifier, functions, constants);
        Ok(Expression::Module(module))
    }

    fn parse_constant(modifiers: Vec<&Modifier>, tokens: &mut Tokens) -> ExpressionResult {
        let identifier = Self::parse_identifier(tokens)?;
        let value = Self::parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err("Invalid constant expression".to_string());
        }
        let mut visibility = Visibility::Private;
        for modifier in modifiers {
            match modifier {
                Modifier::Public => visibility = Visibility::Public,
                Modifier::Private => visibility = Visibility::Private,
                _ => return Err(format!("Invalid modifier: {:?}", modifier)),
            }
        }
        Ok(Expression::Constant(
            visibility,
            identifier,
            Box::from(value),
        ))
    }

    fn parse_path_symbol(symbol: String, tokens: &mut Tokens) -> Result<Path, String> {
        let mut path = Vec::new();
        path.push(symbol);
        let mut last_path_operator = false;
        while let Some(token) = tokens.peek() {
            match token {
                Token::Operator(operator) => match operator {
                    Operator::Path => {
                        if last_path_operator {
                            return Err("Unexpected path operator".to_string());
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

    fn parse_parameters(tokens: &mut Tokens) -> Result<Vec<Parameter>, String> {
        if !Self::is_parameters_section(tokens) {
            return Err("Missing parameters section".to_string());
        }
        let mut tokens = tokens.peekable();
        let mut parameters = Vec::new();
        while let Some(token) = tokens.next() {
            if token.is_parameters_parenthesis() {
                break;
            }
            let name = token.as_symbol()?;
            let next = tokens.peek();
            let parameter = if let Some(Token::Operator(Operator::Variadic)) = next {
                tokens.next();
                Parameter::Variadic(name)
            } else {
                Parameter::Unary(name)
            };
            parameters.push(parameter)
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
