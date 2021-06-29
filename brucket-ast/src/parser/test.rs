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

use crate::ast::constant_value::{Boolean, ConstantValue};
use crate::ast::function::Function;
use crate::ast::{Constant, Identifier, If, Let};

use super::*;

type TestResult = Result<(), Cow<'static, str>>;

#[test]
fn test_parsed_number_token_is_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Numeric(Number::Integer("42".to_string())),
    ));
    let actual = parser.parse(vec![Token::Number(NumberToken::Integer("42".to_string()))])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_boolean_token_is_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Boolean(Boolean::True),
    ));
    let actual = parser.parse(vec![Token::Boolean(BooleanToken::True)])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_string_token_is_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::String("foobar".to_string()),
    ));
    let actual = parser.parse(vec![Token::String("foobar".to_string())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_symbol_token_is_symbol_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Identifier(Identifier::new(
        NodeId(0),
        Path::Simple("foobar".to_string()),
    ));
    let actual = parser.parse(vec![Token::Symbol("foobar".to_string())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_unit_tokens_are_unit_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::ConstantValue(ConstantValue::new(NodeId(0), ConstantVariant::Unit));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_null_token_is_null_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::ConstantValue(ConstantValue::new(NodeId(0), ConstantVariant::Null));
    let actual = parser.parse(vec![Token::Null])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_constant_function_tokens_are_application_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("foobar".to_string()),
        ))),
        Vec::new(),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_unary_function_tokens_are_application_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("foobar".to_string()),
        ))),
        vec![Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_string())),
        ))],
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_multi_parameter_function_tokens_are_application_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("foobar".to_string()),
        ))),
        vec![
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            )),
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("24".to_string())),
            )),
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("0".to_string())),
            )),
        ],
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Number(NumberToken::Integer("24".to_string())),
        Token::Number(NumberToken::Integer("0".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_let_tokens_are_let_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Let(Let::new(
        NodeId(0),
        "x".to_string(),
        Type::Any,
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_string())),
        ))),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_string()),
        ))),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Let),
        Token::Symbol("x".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_if_tokens_are_if_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::If(If::new(
        NodeId(0),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Boolean(Boolean::True),
        ))),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_string())),
        ))),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("24".to_string())),
        ))),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::If),
        Token::Boolean(BooleanToken::True),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Number(NumberToken::Integer("24".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_empty_parameters_lambda_tokens_are_lambda_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Lambda(Lambda::new(
        NodeId(0),
        Vec::new(),
        Type::Any,
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_string()),
        ))),
        // maplit::hashset!("x".to_string()),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_single_parameter_lambda_tokens_are_lambda_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Lambda(Lambda::new(
        NodeId(0),
        vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
        Type::Any,
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_string()),
        ))),
        // HashSet::new(),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_multi_parameters_lambda_tokens_are_lambda_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Lambda(Lambda::new(
        NodeId(0),
        vec![
            Parameter::new("x".to_string(), Type::Any, Arity::Unary),
            Parameter::new("y".to_string(), Type::Any, Arity::Unary),
            Parameter::new("z".to_string(), Type::Any, Arity::Unary),
        ],
        Type::Any,
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_string()),
        ))),
        // HashSet::new(),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_string()),
        Token::Symbol("y".to_string()),
        Token::Symbol("z".to_string()),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_module_tokens_are_module_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Module(Module::new(
        NodeId(0),
        false,
        "foo".to_string(),
        vec![Function::new(
            NodeId(0),
            Visibility::Private,
            ApplicationStrategy::Eager,
            "x".to_string(),
            Lambda::new(
                NodeId(0),
                Vec::new(),
                Type::Any,
                Box::new(Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("42".to_string())),
                ))),
                // HashSet::new(),
            ),
        )],
        vec![InternalFunction::new(
            NodeId(0),
            Visibility::Private,
            ApplicationStrategy::Eager,
            "z".to_string(),
            Vec::new(),
            Type::Any,
        )],
        vec![Constant::new(
            NodeId(0),
            Visibility::Private,
            "y".to_string(),
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
        )],
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Module),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Internal),
        Token::Keyword(Keyword::Function),
        Token::Symbol("z".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Constant),
        Token::Symbol("y".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_function_tokens_are_function_expressions() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Private,
        ApplicationStrategy::Eager,
        "foo".to_string(),
        Lambda::new(
            NodeId(0),
            vec![
                Parameter::new("x".to_string(), Type::Any, Arity::Unary),
                Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                Parameter::new("z".to_string(), Type::Any, Arity::Unary),
            ],
            Type::Any,
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("bar".to_string()),
                ))),
                vec![Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("42".to_string())),
                ))],
            ))),
            // maplit::hashset!("bar".to_string()),
        ),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_string()),
        Token::Symbol("y".to_string()),
        Token::Symbol("z".to_string()),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("bar".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_constant_tokens_are_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Constant(Constant::new(
        NodeId(0),
        Visibility::Private,
        "foo".to_string(),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_string())),
        ))),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Constant),
        Token::Symbol("foo".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_lambda_with_variadic_parameter_tokens_are_lambda_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Lambda(Lambda::new(
        NodeId(0),
        vec![Parameter::new("xs".to_string(), Type::Any, Arity::Variadic)],
        Type::Any,
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_string()),
        ))),
        // maplit::hashset!("x".to_string()),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("xs".to_string()),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_public_function_tokens_are_public_function_expressions() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Public,
        ApplicationStrategy::Eager,
        "foo".to_string(),
        Lambda::new(
            NodeId(0),
            Vec::new(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            // HashSet::new(),
        ),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Public),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_public_constant_tokens_are_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Constant(Constant::new(
        NodeId(0),
        Visibility::Public,
        "foo".to_string(),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_string())),
        ))),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Public),
        Token::Keyword(Keyword::Constant),
        Token::Symbol("foo".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_lazy_function_tokens_are_lazy_function() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Private,
        ApplicationStrategy::Lazy,
        "foo".to_string(),
        Lambda::new(
            NodeId(0),
            Vec::new(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            // HashSet::new(),
        ),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Lazy),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_public_lazy_function_tokens_are_public_lazy_function() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Public,
        ApplicationStrategy::Lazy,
        "foo".to_string(),
        Lambda::new(
            NodeId(0),
            Vec::new(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            // HashSet::new(),
        ),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Public),
        Token::Modifier(Modifier::Lazy),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_internal_function_tokens_are_internal_function() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::InternalFunction(InternalFunction::new(
        NodeId(0),
        Visibility::Public,
        ApplicationStrategy::Lazy,
        "foobar".to_string(),
        vec![
            Parameter::new("x".to_string(), Type::Any, Arity::Unary),
            Parameter::new(
                "y".to_string(),
                Type::Symbol("Bar".to_string()),
                Arity::Unary,
            ),
            Parameter::new("z".to_string(), Type::Integer, Arity::Variadic),
        ],
        Type::Boolean,
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Public),
        Token::Modifier(Modifier::Internal),
        Token::Modifier(Modifier::Lazy),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foobar".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_string()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Any),
        Token::Symbol("y".to_string()),
        Token::Operator(Operator::Type),
        Token::Symbol("Bar".to_string()),
        Token::Symbol("z".to_string()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::PrimitiveType(PrimitiveType::Boolean),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_application_simple_path_identifier_tokens_are_application_expression() -> TestResult
{
    let mut parser = Parser::default();
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Complex(ComplexPath::new(
                "foobar".to_string(),
                vec!["foo".to_string()],
            )),
        ))),
        vec![Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::String("foo".to_string()),
        ))],
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foo".to_string()),
        Token::Operator(Operator::Path),
        Token::Symbol("foobar".to_string()),
        Token::String("foo".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_application_complex_path_identifier_tokens_are_application_expression() -> TestResult
{
    let mut parser = Parser::default();
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Complex(ComplexPath::new(
                "barfoo".to_string(),
                vec!["foo".to_string(), "bar".to_string(), "foobar".to_string()],
            )),
        ))),
        Vec::new(),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foo".to_string()),
        Token::Operator(Operator::Path),
        Token::Symbol("bar".to_string()),
        Token::Operator(Operator::Path),
        Token::Symbol("foobar".to_string()),
        Token::Operator(Operator::Path),
        Token::Symbol("barfoo".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_static_module_tokens_are_module_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Module(Module::new(
        NodeId(0),
        true,
        "foo".to_string(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Static),
        Token::Keyword(Keyword::Module),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_function_with_types_tokens_are_function_expressions() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Private,
        ApplicationStrategy::Eager,
        "foo".to_string(),
        Lambda::new(
            NodeId(0),
            vec![
                Parameter::new("x".to_string(), Type::Any, Arity::Unary),
                Parameter::new(
                    "y".to_string(),
                    Type::Symbol("Bar".to_string()),
                    Arity::Unary,
                ),
                Parameter::new("z".to_string(), Type::Integer, Arity::Variadic),
            ],
            Type::Boolean,
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("bar".to_string()),
                ))),
                vec![Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("42".to_string())),
                ))],
            ))),
            // maplit::hashset!("bar".to_string()),
        ),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_string()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Any),
        Token::Symbol("y".to_string()),
        Token::Operator(Operator::Type),
        Token::Symbol("Bar".to_string()),
        Token::Symbol("z".to_string()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::PrimitiveType(PrimitiveType::Boolean),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("bar".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parse_function_with_lambda_type() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Private,
        ApplicationStrategy::Eager,
        "foobar".to_string(),
        Lambda::new(
            NodeId(0),
            vec![
                Parameter::new("x".to_string(), Type::Any, Arity::Unary),
                Parameter::new(
                    "y".to_string(),
                    Type::Lambda(LambdaType::new(
                        vec![Type::Integer, Type::Boolean],
                        Type::Symbol("Test".to_owned()).into(),
                    )),
                    Arity::Unary,
                ),
                Parameter::new("z".to_string(), Type::Integer, Arity::Variadic),
            ],
            Type::Lambda(LambdaType::new(vec![], Type::Integer.into())),
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("bar".to_string()),
                ))),
                vec![Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("42".to_string())),
                ))],
            ))),
            // maplit::hashset!("bar".to_string()),
        ),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foobar".to_string()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_string()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Any),
        Token::Symbol("y".to_string()),
        Token::Operator(Operator::Type),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::PrimitiveType(PrimitiveType::Boolean),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::Symbol("Test".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Symbol("z".to_string()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("bar".to_string()),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_lambda_type_is_parsed_correctly() -> TestResult {
    let parser = Parser::default();
    assert_eq!(
        Type::Lambda(LambdaType::new(vec![], Type::Integer.into())),
        parser.parse_type(
            &mut vec![
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')')),
            ]
            .into_iter()
            .peekable()
        )?,
    );
    assert_eq!(
        Type::Lambda(LambdaType::new(vec![Type::Integer], Type::Integer.into())),
        parser.parse_type(
            &mut vec![
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')'))
            ]
            .into_iter()
            .peekable()
        )?,
    );
    assert_eq!(
        Type::Lambda(LambdaType::new(
            vec![Type::Integer, Type::Integer],
            Type::Integer.into()
        )),
        parser.parse_type(
            &mut vec![
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')'))
            ]
            .into_iter()
            .peekable()
        )?,
    );
    assert_eq!(
        Type::Lambda(LambdaType::new(
            vec![
                Type::Integer,
                Type::Lambda(LambdaType::new(vec![Type::Integer], Type::Integer.into()))
            ],
            Type::Integer.into()
        )),
        parser.parse_type(
            &mut vec![
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')')),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')'))
            ]
            .into_iter()
            .peekable()
        )?,
    );
    Ok(())
}

#[test]
fn test_typed_let_is_let_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = Node::Let(Let::new(
        NodeId(0),
        "x".to_string(),
        Type::Integer,
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_string())),
        ))),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_string()),
        ))),
    ));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Let),
        Token::Symbol("x".to_string()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Number(NumberToken::Integer("42".to_string())),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}
