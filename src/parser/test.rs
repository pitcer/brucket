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

type TestResult = Result<(), String>;

#[test]
fn test_parsed_number_token_is_constant_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Constant(Constant::Numeric(42));
    let actual = parser.parse(&[Token::Number(42)])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_boolean_token_is_constant_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Constant(Constant::Boolean(true));
    let actual = parser.parse(&[Token::Boolean(true)])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_string_token_is_constant_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Constant(Constant::String("foobar".to_string()));
    let actual = parser.parse(&[Token::String("foobar".to_string())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_symbol_token_is_symbol_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Identifier("foobar".to_string());
    let actual = parser.parse(&[Token::Symbol("foobar".to_string())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_unit_tokens_are_unit_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Constant(Constant::Unit);
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_null_token_is_null_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Constant(Constant::Null);
    let actual = parser.parse(&[Token::Null])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_constant_function_tokens_are_application_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Application(
        Box::new(Expression::Identifier("foobar".to_string())),
        Vec::new(),
    );
    let actual = parser.parse(&[
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
    let expected = Expression::Application(
        Box::new(Expression::Identifier("foobar".to_string())),
        vec![Expression::Constant(Constant::Numeric(42))],
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_string()),
        Token::Number(42),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_multi_parameter_function_tokens_are_application_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Application(
        Box::new(Expression::Identifier("foobar".to_string())),
        vec![
            Expression::Constant(Constant::Numeric(42)),
            Expression::Constant(Constant::Numeric(24)),
            Expression::Constant(Constant::Numeric(0)),
        ],
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_string()),
        Token::Number(42),
        Token::Number(24),
        Token::Number(0),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_let_tokens_are_let_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Identifier("x".to_string())),
    );
    let actual = parser.parse(&[
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

#[test]
fn test_parsed_letrec_tokens_are_letrec_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Letrec(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Identifier("x".to_string())),
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Letrec),
        Token::Symbol("x".to_string()),
        Token::Number(42),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_if_tokens_are_if_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::If(
        Box::new(Expression::Constant(Constant::Boolean(true))),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Constant(Constant::Numeric(24))),
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::If),
        Token::Boolean(true),
        Token::Number(42),
        Token::Number(24),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_empty_parameters_lambda_tokens_are_lambda_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Lambda(
        Vec::new(),
        Box::new(Expression::Identifier("x".to_string())),
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_single_parameter_lambda_tokens_are_lambda_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Lambda(
        vec![Parameter::Unary("x".to_string())],
        Box::new(Expression::Identifier("x".to_string())),
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_multi_parameters_lambda_tokens_are_lambda_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Lambda(
        vec![
            Parameter::Unary("x".to_string()),
            Parameter::Unary("y".to_string()),
            Parameter::Unary("z".to_string()),
        ],
        Box::new(Expression::Identifier("x".to_string())),
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Symbol("x".to_string()),
        Token::Symbol("y".to_string()),
        Token::Symbol("z".to_string()),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_internal_tokens_are_internal_call_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::InternalCall(
        "foo".to_string(),
        vec![
            Expression::Identifier("x".to_string()),
            Expression::Constant(Constant::Numeric(42)),
            Expression::Identifier("y".to_string()),
        ],
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Internal),
        Token::Symbol("foo".to_string()),
        Token::Symbol("x".to_string()),
        Token::Number(42),
        Token::Symbol("y".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_module_tokens_are_module_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Module(
        "foo".to_string(),
        vec![
            Expression::Identifier("x".to_string()),
            Expression::Identifier("y".to_string()),
            Expression::Identifier("z".to_string()),
        ],
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Module),
        Token::Symbol("foo".to_string()),
        Token::Symbol("x".to_string()),
        Token::Symbol("y".to_string()),
        Token::Symbol("z".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_function_tokens_are_identified_lambda_expressions() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Identified(
        "foo".to_string(),
        Box::new(Expression::Lambda(
            vec![
                Parameter::Unary("x".to_string()),
                Parameter::Unary("y".to_string()),
                Parameter::Unary("z".to_string()),
            ],
            Box::new(Expression::Application(
                Box::new(Expression::Identifier("bar".to_string())),
                vec![Expression::Constant(Constant::Numeric(42))],
            )),
        )),
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Symbol("x".to_string()),
        Token::Symbol("y".to_string()),
        Token::Symbol("z".to_string()),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("bar".to_string()),
        Token::Number(42),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_constant_tokens_are_identified_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Identified(
        "foo".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Constant),
        Token::Symbol("foo".to_string()),
        Token::Number(42),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_and_tokens_are_and_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::And(vec![
        Expression::Constant(Constant::Boolean(true)),
        Expression::Constant(Constant::Numeric(42)),
        Expression::Constant(Constant::String("foobar".to_string())),
    ]);
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::And),
        Token::Boolean(true),
        Token::Number(42),
        Token::String("foobar".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_or_tokens_are_or_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Or(vec![
        Expression::Constant(Constant::Boolean(true)),
        Expression::Constant(Constant::Numeric(42)),
        Expression::Constant(Constant::String("foobar".to_string())),
    ]);
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Or),
        Token::Boolean(true),
        Token::Number(42),
        Token::String("foobar".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_lambda_with_variadic_parameter_tokens_are_lambda_expression() -> TestResult {
    let parser = Parser::default();
    let expected = Expression::Lambda(
        vec![Parameter::Variadic("xs".to_string())],
        Box::new(Expression::Identifier("x".to_string())),
    );
    let actual = parser.parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Symbol("xs".to_string()),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Parameters),
        Token::Symbol("x".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}
