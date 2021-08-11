use crate::token::{
    Boolean as BooleanToken, Keyword, Modifier, Number as NumberToken, Operator, Parenthesis,
    PrimitiveType, Token,
};
use brucket_ast::ast_type::{LambdaType, Type};
use brucket_ast::constant_value::{Boolean, ConstantValue, ConstantVariant, Number};
use brucket_ast::function::{ApplicationStrategy, Function, InternalFunction};
use brucket_ast::lambda::{Arity, Lambda, Parameter};
use brucket_ast::path::Path;
use brucket_ast::{Application, Constant, Identifier, If, Let, Module, Node, NodeId, Visibility};
use derive_more::Constructor;
use std::borrow::Cow;
use std::iter::Peekable;
use std::option::Option::Some;
use std::vec::IntoIter;

#[cfg(test)]
mod tests;

type NodeResult = ParseResult<Node>;
pub type ParseResult<T> = Result<T, ParseError>;
pub type ParseError = Cow<'static, str>;

impl Token {
    fn into_symbol(self) -> ParseResult<String> {
        match self {
            Token::Symbol(symbol) => Ok(symbol),
            _ => Err(Cow::from(format!("Token is not a symbol: {:?}", self))),
        }
    }

    fn is_open_parenthesis(&self) -> bool {
        matches!(*self, Token::Parenthesis(Parenthesis::Open(_)))
    }

    fn is_close_parenthesis(&self) -> bool {
        matches!(*self, Token::Parenthesis(Parenthesis::Close(_)))
    }
}

type Tokens = Peekable<IntoIter<Token>>;

#[derive(Default, Constructor)]
pub struct Parser {
    current_id: NodeId,
}

impl Parser {
    #[inline]
    pub fn parse(&mut self, tokens: Vec<Token>) -> NodeResult {
        let mut iterator = tokens.into_iter().peekable();
        self.parse_first(&mut iterator)
    }

    #[cfg(not(test))]
    fn create_node_id(&mut self) -> NodeId {
        let node_id = self.current_id;
        self.current_id = NodeId(node_id.0 + 1);
        node_id
    }

    #[cfg(test)]
    fn create_node_id(&mut self) -> NodeId {
        self.current_id
    }

    fn parse_first(&mut self, tokens: &mut Tokens) -> NodeResult {
        match tokens.next() {
            Some(token) => self.parse_first_token(token, tokens),
            None => Err(Cow::from("Empty tokens")),
        }
    }

    fn parse_first_token(&mut self, token: Token, tokens: &mut Tokens) -> NodeResult {
        match token {
            Token::Parenthesis(parenthesis) => self.parse_parenthesis(&parenthesis, tokens),
            Token::Operator(operator) => Self::parse_operator(&operator),
            Token::Null => Ok(self.create_constant_value(ConstantVariant::Null)),
            Token::String(value) => Ok(self.create_constant_value(ConstantVariant::String(value))),
            Token::Number(value) => {
                let number = Self::parse_number(value);
                let variant = ConstantVariant::Numeric(number);
                Ok(self.create_constant_value(variant))
            }
            Token::Boolean(value) => {
                let boolean = Self::parse_boolean(&value);
                let variant = ConstantVariant::Boolean(boolean);
                Ok(self.create_constant_value(variant))
            }
            Token::Keyword(keyword) => Err(Cow::from(format!("Unexpected token: {:?}", keyword))),
            Token::Symbol(symbol) => {
                let path = Self::parse_path_symbol(symbol, tokens)?;
                Ok(self.create_identifier(path))
            }
            Token::Modifier(modifier) => {
                Err(Cow::from(format!("Unexpected token: {:?}", modifier)))
            }
            Token::PrimitiveType(type_token) => {
                Err(Cow::from(format!("Unexpected token: {:?}", type_token)))
            }
        }
    }

    fn parse_parenthesis(&mut self, parenthesis: &Parenthesis, tokens: &mut Tokens) -> NodeResult {
        match *parenthesis {
            Parenthesis::Open(_) => self.parse_section(tokens),
            Parenthesis::Close(_) => Err(Cow::from("Unexpected close parenthesis")),
        }
    }

    fn parse_operator(operator: &Operator) -> NodeResult {
        match *operator {
            Operator::Variadic => Err(Cow::from("Unexpected variadic operator")),
            Operator::Path => Err(Cow::from("Unexpected path operator")),
            Operator::Type => Err(Cow::from("Unexpected type operator")),
            Operator::SkinnyArrowRight => Err(Cow::from("Unexpected '->'")),
            Operator::ThickArrowRight => Err(Cow::from("Unexpected '=>'")),
        }
    }

    fn create_constant_value(&mut self, variant: ConstantVariant) -> Node {
        let node_id = self.create_node_id();
        let constant_value = ConstantValue::new(node_id, variant);
        Node::ConstantValue(constant_value)
    }

    fn parse_number(token: NumberToken) -> Number {
        match token {
            NumberToken::Integer(value) => Number::Integer(value),
            NumberToken::FloatingPoint(value) => Number::FloatingPoint(value),
        }
    }

    fn parse_boolean(token: &BooleanToken) -> Boolean {
        match *token {
            BooleanToken::True => Boolean::True,
            BooleanToken::False => Boolean::False,
        }
    }

    fn parse_section(&mut self, tokens: &mut Tokens) -> NodeResult {
        match tokens.next() {
            Some(token) => self.parse_section_token(token, tokens),
            None => Err(Cow::from("Empty tokens")),
        }
    }

    fn parse_section_token(&mut self, token: Token, tokens: &mut Tokens) -> NodeResult {
        match token {
            Token::Parenthesis(parenthesis) => match parenthesis {
                Parenthesis::Open(_) => {
                    let identifier = self.parse_section(tokens)?;
                    self.parse_application(identifier, tokens)
                }
                Parenthesis::Close(_) => Ok(self.create_constant_value(ConstantVariant::Unit)),
            },
            Token::Keyword(keyword) => self.parse_keyword(tokens, &keyword),
            Token::Modifier(modifier) => self.parse_modifiers(modifier, tokens),
            Token::Symbol(symbol) => {
                let path = Self::parse_path_symbol(symbol, tokens)?;
                let identifier = self.create_identifier(path);
                self.parse_application(identifier, tokens)
            }
            Token::Operator(operator) => match operator {
                Operator::ThickArrowRight => self
                    .create_lambda(tokens)
                    .map(|lambda| Node::Lambda(Box::new(lambda))),
                _ => Err(Cow::from(format!("Invalid operator: {:?}", operator))),
            },
            _ => Err(Cow::from(format!("Invalid token: {:?}", token))),
        }
    }

    fn create_identifier(&mut self, path: Path) -> Node {
        let node_id = self.create_node_id();
        let identifier = Identifier::new(node_id, path);
        Node::Identifier(identifier)
    }

    fn parse_keyword(&mut self, tokens: &mut Tokens, keyword: &Keyword) -> NodeResult {
        match *keyword {
            Keyword::Let => self.create_let(tokens),
            Keyword::If => self.create_if(tokens),
            Keyword::Lambda => self
                .create_lambda(tokens)
                .map(|lambda| Node::Lambda(Box::new(lambda))),
            Keyword::Module => self.create_module(Vec::new(), tokens),
            Keyword::Function => self.create_function(Vec::new(), tokens),
            Keyword::Constant => self.create_constant(Vec::new(), tokens),
        }
    }

    fn parse_application(&mut self, identifier: Node, tokens: &mut Tokens) -> NodeResult {
        let arguments = self.parse_arguments(tokens)?;
        let node_id = self.create_node_id();
        let application = Application::new(node_id, identifier, arguments);
        Ok(Node::Application(Box::new(application)))
    }

    fn create_let(&mut self, tokens: &mut Tokens) -> NodeResult {
        let name = Self::parse_identifier(tokens)?;
        let next = tokens.peek();
        if let Some(token) = next {
            let value_type = match *token {
                Token::Operator(Operator::Type) => {
                    tokens.next();
                    self.parse_type(tokens)?
                }
                _ => Type::Any,
            };
            let value = self.parse_first(tokens)?;
            let then = self.parse_first(tokens)?;
            if !Self::is_section_closed(tokens) {
                return Err(Cow::from("Invalid let expression"));
            }
            let node_id = self.create_node_id();
            let let_node = Let::new(node_id, name, value_type, value, then);
            Ok(Node::Let(Box::new(let_node)))
        } else {
            Err(Cow::from("Invalid let expression: end of tokens"))
        }
    }

    fn create_if(&mut self, tokens: &mut Tokens) -> NodeResult {
        let condition = self.parse_first(tokens)?;
        let if_true_then = self.parse_first(tokens)?;
        let if_false_then = self.parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid if expression"));
        }
        let node_id = self.create_node_id();
        let if_node = If::new(node_id, condition, if_true_then, if_false_then);
        Ok(Node::If(Box::new(if_node)))
    }

    fn parse_modifiers(&mut self, first_modifier: Modifier, tokens: &mut Tokens) -> NodeResult {
        let mut modifiers = vec![first_modifier];
        while let Some(token) = tokens.next() {
            if let Token::Modifier(modifier) = token {
                modifiers.push(modifier);
            } else {
                return self.parse_with_modifiers(token, modifiers, tokens);
            }
        }
        Err(Cow::from("Invalid token"))
    }

    fn parse_with_modifiers(
        &mut self,
        current_token: Token,
        modifiers: Vec<Modifier>,
        tokens: &mut Tokens,
    ) -> NodeResult {
        match current_token {
            Token::Keyword(keyword) => match keyword {
                Keyword::Function => self.create_function(modifiers, tokens),
                Keyword::Constant => self.create_constant(modifiers, tokens),
                Keyword::Module => self.create_module(modifiers, tokens),
                _ => Err(Cow::from("Invalid token")),
            },
            _ => Err(Cow::from("Invalid token")),
        }
    }

    fn create_function(&mut self, modifiers: Vec<Modifier>, tokens: &mut Tokens) -> NodeResult {
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
                    return Err(Cow::from("Static is an invalid modifier for a function"))
                }
            }
        }
        if internal {
            self.create_internal_function(tokens, identifier, visibility, application_strategy)
        } else {
            self.create_lambda_function(tokens, identifier, visibility, application_strategy)
        }
    }

    fn create_lambda_function(
        &mut self,
        tokens: &mut Tokens,
        identifier: String,
        visibility: Visibility,
        application_strategy: ApplicationStrategy,
    ) -> NodeResult {
        let lambda = self.create_lambda(tokens)?;
        let node_id = self.create_node_id();
        let function = Function::new(
            node_id,
            visibility,
            application_strategy,
            identifier,
            lambda,
        );
        Ok(Node::Function(Box::new(function)))
    }

    fn create_internal_function(
        &mut self,
        tokens: &mut Tokens,
        identifier: String,
        visibility: Visibility,
        application_strategy: ApplicationStrategy,
    ) -> NodeResult {
        let parameters = self.parse_parameters(tokens)?;
        let return_type = self.parse_return_type(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid internal function expression"));
        }
        let node_id = self.create_node_id();
        let internal_function = InternalFunction::new(
            node_id,
            visibility,
            application_strategy,
            identifier,
            parameters,
            return_type,
        );
        Ok(Node::InternalFunction(internal_function))
    }

    fn create_lambda(&mut self, tokens: &mut Tokens) -> ParseResult<Lambda> {
        let parameters = self.parse_parameters(tokens)?;
        let return_type = self.parse_return_type(tokens)?;
        let body = self.parse_first(tokens)?;
        if !Self::is_section_closed(tokens) {
            return Err(Cow::from("Invalid lambda expression"));
        }
        let node_id = self.create_node_id();
        let lambda = Lambda::new(node_id, parameters, return_type, body);
        Ok(lambda)
    }

    fn parse_return_type(&mut self, tokens: &mut Tokens) -> ParseResult<Type> {
        let next = tokens.peek();
        let token = next.ok_or("An unexpected end of tokens")?;
        match *token {
            Token::Operator(Operator::SkinnyArrowRight) => {
                tokens.next();
                self.parse_type(tokens)
            }
            _ => Ok(Type::Any),
        }
    }

    fn parse_type(&self, tokens: &mut Tokens) -> ParseResult<Type> {
        let next = tokens.next();
        let token = next.ok_or("End of tokens")?;
        self.parse_type_token(token, tokens)
    }

    fn parse_type_token(&self, token: Token, tokens: &mut Tokens) -> ParseResult<Type> {
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
            Token::Parenthesis(Parenthesis::Open('(')) => self
                .parse_lambda_type(tokens)
                .map(|lambda| Type::Lambda(Box::new(lambda))),
            _ => Err(Cow::from("Invalid type token")),
        }
    }

    fn parse_lambda_type(&self, tokens: &mut Tokens) -> ParseResult<LambdaType> {
        let mut parameters_types = Vec::new();
        while let Some(token) = tokens.next() {
            if token == Token::Operator(Operator::SkinnyArrowRight) {
                break;
            }
            let parameter_type = self.parse_type_token(token, tokens)?;
            parameters_types.push(parameter_type);
        }
        let return_type = self.parse_type(tokens)?;
        if let Some(Token::Parenthesis(Parenthesis::Close(')'))) = tokens.next() {
            Ok(LambdaType::new(parameters_types, return_type))
        } else {
            Err(Cow::from("Missing close parenthesis in lambda type"))
        }
    }

    fn create_module(
        &mut self,
        modifiers: Vec<Modifier>,
        tokens: &mut Tokens,
    ) -> ParseResult<Node> {
        let identifier = Self::parse_identifier(tokens)?;
        let mut functions = Vec::new();
        let mut internal_functions = Vec::new();
        let mut constants = Vec::new();
        while let Some(token) = tokens.next() {
            if token.is_close_parenthesis() {
                break;
            }
            let member = self.parse_first_token(token, tokens)?;
            match member {
                Node::Function(function) => functions.push(*function),
                Node::InternalFunction(function) => internal_functions.push(function),
                Node::Constant(constant) => constants.push(*constant),
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
        let node_id = self.create_node_id();
        let module = Module::new(
            node_id,
            is_static,
            identifier,
            functions,
            internal_functions,
            constants,
        );
        Ok(Node::Module(module))
    }

    fn parse_path_symbol(symbol: String, tokens: &mut Tokens) -> ParseResult<Path> {
        let mut path = vec![symbol];
        let mut last_path_operator = false;
        while let Some(token) = tokens.peek() {
            match *token {
                Token::Operator(Operator::Path) => {
                    if last_path_operator {
                        return Err(Cow::from("Unexpected path operator"));
                    }
                    last_path_operator = true;
                    tokens.next();
                }
                Token::Symbol(ref symbol) => {
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
        if length == 1 {
            Ok(Path::Simple(path.remove(0)))
        } else {
            Ok(Path::Complex(path))
        }
    }

    fn create_constant(&mut self, modifiers: Vec<Modifier>, tokens: &mut Tokens) -> NodeResult {
        let identifier = Self::parse_identifier(tokens)?;
        let value = self.parse_first(tokens)?;
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
        let node_id = self.create_node_id();
        let constant = Constant::new(node_id, visibility, identifier, value);
        Ok(Node::Constant(Box::new(constant)))
    }

    fn parse_identifier(tokens: &mut Tokens) -> ParseResult<String> {
        let identifier = tokens.next();
        identifier.map_or_else(|| Err(Cow::from("Missing name token")), Token::into_symbol)
    }

    fn parse_arguments(&mut self, tokens: &mut Tokens) -> ParseResult<Vec<Node>> {
        let mut arguments = Vec::new();
        while let Some(token) = tokens.next() {
            if token.is_close_parenthesis() {
                break;
            }
            let argument = self.parse_first_token(token, tokens)?;
            arguments.push(argument);
        }
        Ok(arguments)
    }

    fn parse_parameters(&mut self, tokens: &mut Tokens) -> ParseResult<Vec<Parameter>> {
        if !Self::is_section_opened(tokens) {
            return Err(Cow::from("Missing parameters section"));
        }
        let mut parameters = Vec::new();
        while let Some(token) = tokens.next() {
            if token.is_close_parenthesis() {
                break;
            }
            let parameter = self.parse_parameter(token, tokens)?;
            parameters.push(parameter);
        }
        Ok(parameters)
    }

    fn parse_parameter(&mut self, token: Token, tokens: &mut Tokens) -> ParseResult<Parameter> {
        let name = token.into_symbol()?;
        let next = tokens.peek();
        if let Some(token) = next {
            match *token {
                Token::Operator(ref operator) => match *operator {
                    Operator::Variadic => {
                        tokens.next();
                        Ok(Parameter::new(name, Type::Any, Arity::Variadic))
                    }
                    Operator::Type => {
                        tokens.next();
                        let parameter_type = self.parse_type(tokens)?;
                        let next = tokens.peek();
                        let arity = if let Some(next) = next {
                            if let Token::Operator(Operator::Variadic) = *next {
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
