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
    let expected = Expression::Constant(42);
    let actual = parse(&[Token::Number(42)])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_boolean_token_is_boolean_expression() -> TestResult {
    let expected = Expression::Boolean(true);
    let actual = parse(&[Token::Boolean(true)])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_string_token_is_string_expression() -> TestResult {
    let expected = Expression::String("foobar".to_string());
    let actual = parse(&[Token::String("foobar".to_string())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_symbol_token_is_symbol_expression() -> TestResult {
    let expected = Expression::Symbol("foobar".to_string());
    let actual = parse(&[Token::Symbol("foobar".to_string())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_unit_function_tokens_are_function_expression() -> TestResult {
    let expected = Expression::Function(Function::Unit);
    let actual = parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_constant_function_tokens_are_function_expression() -> TestResult {
    let expected = Expression::Function(Function::Constant(Box::new(Expression::Symbol(
        "foobar".to_string(),
    ))));
    let actual = parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_function_tokens_are_function_expression() -> TestResult {
    let expected = Expression::Function(Function::NAry(
        Box::new(Expression::Symbol("foobar".to_string())),
        vec![Expression::Constant(42), Expression::Constant(24)],
    ));
    let actual = parse(&[
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_string()),
        Token::Number(42),
        Token::Number(24),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_let_tokens_are_let_expression() -> TestResult {
    let expected = Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(42)),
        Box::new(Expression::Symbol("x".to_string())),
    );
    let actual = parse(&[
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
