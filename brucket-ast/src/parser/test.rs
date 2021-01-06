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

use super::*;
use crate::ast::{Boolean, Constant, Function, If, Let};

type TestResult = Result<(), Cow<'static, str>>;

#[test]
fn test_parsed_number_token_is_constant_expression() -> TestResult {
    let parser = Parser::default();
    let expected =
        Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("42".to_string())));
    let actual = parser.parse(vec![Token::Number(NumberToken::Integer("42".to_string()))])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_boolean_token_is_constant_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::ConstantValue(ConstantValue::Boolean(Boolean::True));
    let actual = parser.parse(vec![Token::Boolean(BooleanToken::True)])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_string_token_is_constant_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::ConstantValue(ConstantValue::String("foobar".to_string()));
    let actual = parser.parse(vec![Token::String("foobar".to_string())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_symbol_token_is_symbol_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Identifier(Path::Simple("foobar".to_string()));
    let actual = parser.parse(vec![Token::Symbol("foobar".to_string())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_unit_tokens_are_unit_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::ConstantValue(ConstantValue::Unit);
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_null_token_is_null_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::ConstantValue(ConstantValue::Null);
    let actual = parser.parse(vec![Token::Null])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_constant_function_tokens_are_application_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Application(Application {
        identifier: Box::new(Expression::Identifier(Path::Simple("foobar".to_string()))),
        arguments: Vec::new(),
    });
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
    let parser = Parser::default();
    let expected = Expression::Application(Application {
        identifier: Box::new(Expression::Identifier(Path::Simple("foobar".to_string()))),
        arguments: vec![Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))],
    });
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
    let parser = Parser::default();
    let expected = Expression::Application(Application {
        identifier: Box::new(Expression::Identifier(Path::Simple("foobar".to_string()))),
        arguments: vec![
            Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("42".to_string()))),
            Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("24".to_string()))),
            Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("0".to_string()))),
        ],
    });
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
    let parser = Parser::default();
    let expected = Expression::Let(Let {
        name: "x".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        then: Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
    });
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
    let parser = Parser::default();
    let expected = Expression::If(If::new(
        Box::new(Expression::ConstantValue(ConstantValue::Boolean(
            Boolean::True,
        ))),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("24".to_string()),
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
    let parser = Parser::default();
    let expected = Expression::Lambda(Lambda::new(
        Vec::new(),
        Type::Any,
        Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
        maplit::hashset!("x".to_string()),
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
    let parser = Parser::default();
    let expected = Expression::Lambda(Lambda::new(
        vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
        Type::Any,
        Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
        HashSet::new(),
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
    let parser = Parser::default();
    let expected = Expression::Lambda(Lambda::new(
        vec![
            Parameter::new("x".to_string(), Type::Any, Arity::Unary),
            Parameter::new("y".to_string(), Type::Any, Arity::Unary),
            Parameter::new("z".to_string(), Type::Any, Arity::Unary),
        ],
        Type::Any,
        Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
        HashSet::new(),
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
    let parser = Parser::default();
    let expected = Expression::Module(Module::new(
        false,
        "foo".to_string(),
        vec![Function {
            visibility: Visibility::Private,
            application_strategy: ApplicationStrategy::Eager,
            name: "x".to_string(),
            body: Lambda::new(
                Vec::new(),
                Type::Any,
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                    Number::Integer("42".to_string()),
                ))),
                HashSet::new(),
            ),
        }],
        vec![InternalFunction::new(
            Visibility::Private,
            ApplicationStrategy::Eager,
            "z".to_string(),
            Vec::new(),
            Type::Any,
        )],
        vec![Constant {
            visibility: Visibility::Private,
            name: "y".to_string(),
            value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("42".to_string()),
            ))),
        }],
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
    let parser = Parser::default();
    let expected = Expression::Function(Function {
        visibility: Visibility::Private,
        application_strategy: ApplicationStrategy::Eager,
        name: "foo".to_string(),
        body: Lambda::new(
            vec![
                Parameter::new("x".to_string(), Type::Any, Arity::Unary),
                Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                Parameter::new("z".to_string(), Type::Any, Arity::Unary),
            ],
            Type::Any,
            Box::new(Expression::Application(Application {
                identifier: Box::new(Expression::Identifier(Path::Simple("bar".to_string()))),
                arguments: vec![Expression::ConstantValue(ConstantValue::Numeric(
                    Number::Integer("42".to_string()),
                ))],
            })),
            maplit::hashset!("bar".to_string()),
        ),
    });
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
    let parser = Parser::default();
    let expected = Expression::Constant(Constant {
        visibility: Visibility::Private,
        name: "foo".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
    });
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
    let parser = Parser::default();
    let expected = Expression::Lambda(Lambda::new(
        vec![Parameter::new("xs".to_string(), Type::Any, Arity::Variadic)],
        Type::Any,
        Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
        maplit::hashset!("x".to_string()),
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
    let parser = Parser::default();
    let expected = Expression::Function(Function {
        visibility: Visibility::Public,
        application_strategy: ApplicationStrategy::Eager,
        name: "foo".to_string(),
        body: Lambda::new(
            Vec::new(),
            Type::Any,
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("42".to_string()),
            ))),
            HashSet::new(),
        ),
    });
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
    let parser = Parser::default();
    let expected = Expression::Constant(Constant {
        visibility: Visibility::Public,
        name: "foo".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
    });
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
    let parser = Parser::default();
    let expected = Expression::Function(Function {
        visibility: Visibility::Private,
        application_strategy: ApplicationStrategy::Lazy,
        name: "foo".to_string(),
        body: Lambda::new(
            Vec::new(),
            Type::Any,
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("42".to_string()),
            ))),
            HashSet::new(),
        ),
    });
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
    let parser = Parser::default();
    let expected = Expression::Function(Function {
        visibility: Visibility::Public,
        application_strategy: ApplicationStrategy::Lazy,
        name: "foo".to_string(),
        body: Lambda::new(
            Vec::new(),
            Type::Any,
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("42".to_string()),
            ))),
            HashSet::new(),
        ),
    });
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
    let parser = Parser::default();
    let expected = Expression::InternalFunction(InternalFunction::new(
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
    let parser = Parser::default();
    let expected = Expression::Application(Application {
        identifier: Box::new(Expression::Identifier(Path::Complex(ComplexPath::new(
            "foobar".to_string(),
            vec!["foo".to_string()],
        )))),
        arguments: vec![Expression::ConstantValue(ConstantValue::String(
            "foo".to_string(),
        ))],
    });
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
    let parser = Parser::default();
    let expected = Expression::Application(Application {
        identifier: Box::new(Expression::Identifier(Path::Complex(ComplexPath::new(
            "barfoo".to_string(),
            vec!["foo".to_string(), "bar".to_string(), "foobar".to_string()],
        )))),
        arguments: Vec::new(),
    });
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
    let parser = Parser::default();
    let expected = Expression::Module(Module::new(
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
    let parser = Parser::default();
    let expected = Expression::Function(Function {
        visibility: Visibility::Private,
        application_strategy: ApplicationStrategy::Eager,
        name: "foo".to_string(),
        body: Lambda::new(
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
            Box::new(Expression::Application(Application {
                identifier: Box::new(Expression::Identifier(Path::Simple("bar".to_string()))),
                arguments: vec![Expression::ConstantValue(ConstantValue::Numeric(
                    Number::Integer("42".to_string()),
                ))],
            })),
            maplit::hashset!("bar".to_string()),
        ),
    });
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
