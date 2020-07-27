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
fn test_evaluated_constant_expression_is_numeric_value() -> TestResult {
    let expected = Value::Numeric(42);
    let actual = evaluate(&Expression::Constant(42))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_boolean_expression_is_boolean_value() -> TestResult {
    let expected = Value::Boolean(true);
    let actual = evaluate(&Expression::Boolean(true))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_unit_function_expression_is_unit_value() -> TestResult {
    let expected = Value::Unit;
    let actual = evaluate(&Expression::Function(Function::Unit))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_addition_function_expression_is_numeric_value() -> TestResult {
    let expected = Value::Numeric(42 + 24);
    let actual = evaluate(&Expression::Function(Function::NAry(
        Box::new(Expression::Symbol("+".to_string())),
        vec![Expression::Constant(42), Expression::Constant(24)],
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_let_expression_variable_has_correct_value() -> TestResult {
    let expected = Value::Numeric(42);
    let actual = evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(42)),
        Box::new(Expression::Symbol("x".to_string())),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
#[should_panic(expected = "Cannot evaluate function argument: \"Undefined variable: x\"")]
fn test_environment_is_cleared() {
    let _result = evaluate(&Expression::Function(Function::NAry(
        Box::new(Expression::Symbol("+".to_string())),
        vec![
            Expression::Let(
                "x".to_string(),
                Box::new(Expression::Constant(42)),
                Box::new(Expression::Symbol("x".to_string())),
            ),
            Expression::Symbol("x".to_string()),
        ],
    )));
}

#[test]
fn test_first_variable_is_not_overwritten_by_second_with_different_name() -> TestResult {
    let expected = Value::Numeric(40);
    let actual = evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(40)),
        Box::new(Expression::Let(
            "y".to_string(),
            Box::new(Expression::Constant(2)),
            Box::new(Expression::Symbol("x".to_string())),
        )),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_overwritten_by_second_with_the_same_name() -> TestResult {
    let expected = Value::Numeric(2);
    let actual = evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(40)),
        Box::new(Expression::Let(
            "x".to_string(),
            Box::new(Expression::Constant(2)),
            Box::new(Expression::Symbol("x".to_string())),
        )),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_two_variables_can_be_used_in_function_evaluation() -> TestResult {
    let expected = Value::Numeric(42);
    let actual = evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(40)),
        Box::new(Expression::Let(
            "y".to_string(),
            Box::new(Expression::Constant(2)),
            Box::new(Expression::Function(Function::NAry(
                Box::new(Expression::Symbol("+".to_string())),
                vec![
                    Expression::Symbol("x".to_string()),
                    Expression::Symbol("y".to_string()),
                ],
            ))),
        )),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}
