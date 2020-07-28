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
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(42);
    let actual = evaluator.evaluate(&Expression::Constant(Constant::Numeric(42)))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_boolean_expression_is_boolean_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Boolean(true);
    let actual = evaluator.evaluate(&Expression::Constant(Constant::Boolean(true)))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_string_expression_is_textual_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Textual("foobar".to_string());
    let actual = evaluator.evaluate(&Expression::Constant(Constant::String(
        "foobar".to_string(),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_unit_function_expression_is_unit_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Unit;
    let actual = evaluator.evaluate(&Expression::Constant(Constant::Unit))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_let_expression_variable_has_correct_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(42);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Identifier("x".to_string())),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_not_overwritten_by_second_with_different_name() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(40);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(40))),
        Box::new(Expression::Let(
            "y".to_string(),
            Box::new(Expression::Constant(Constant::Numeric(2))),
            Box::new(Expression::Identifier("x".to_string())),
        )),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_overwritten_by_second_with_the_same_name() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(2);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(40))),
        Box::new(Expression::Let(
            "x".to_string(),
            Box::new(Expression::Constant(Constant::Numeric(2))),
            Box::new(Expression::Identifier("x".to_string())),
        )),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_if_expression_has_correct_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(42);
    let actual = evaluator.evaluate(&Expression::If(
        Box::new(Expression::Constant(Constant::Boolean(true))),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Constant(Constant::Numeric(24))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_empty_lambda_expression_is_empty_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::Empty(
        Expression::Identifier("x".to_string()),
        maplit::hashmap! {"x".to_string() => Value::Numeric(42)},
    ));
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Lambda(Lambda::Empty(Box::new(
            Expression::Identifier("x".to_string()),
        )))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_parametrized_lambda_expression_is_parametrized_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::Parametrized(
        "y".to_string(),
        Expression::Identifier("y".to_string()),
        maplit::hashmap! {"x".to_string() => Value::Numeric(42)},
    ));
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Lambda(Lambda::Parametrized(
            "y".to_string(),
            Box::new(Expression::Identifier("y".to_string())),
        ))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_empty_call_on_empty_lambda_expression_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(42);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Call(Call::Empty(Box::new(Expression::Lambda(
            Lambda::Empty(Box::new(Expression::Identifier("x".to_string()))),
        ))))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_unary_call_on_parametrized_lambda_expression_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(24);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Call(Call::Unary(
            Box::new(Expression::Lambda(Lambda::Parametrized(
                "y".to_string(),
                Box::new(Expression::Identifier("y".to_string())),
            ))),
            Box::new(Expression::Constant(Constant::Numeric(24))),
        ))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_closure_does_not_have_access_to_variable_outside_its_environment() {
    let evaluator = Evaluator::default();
    let expected = Err("Undefined variable: z".to_string());
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::Constant(Constant::Numeric(42))),
        Box::new(Expression::Let(
            "y".to_string(),
            Box::new(Expression::Lambda(Lambda::Parametrized(
                "a".to_string(),
                Box::new(Expression::Identifier("z".to_string())),
            ))),
            Box::new(Expression::Let(
                "z".to_string(),
                Box::new(Expression::Constant(Constant::Numeric(24))),
                Box::new(Expression::Call(Call::Unary(
                    Box::new(Expression::Identifier("y".to_string())),
                    Box::new(Expression::Constant(Constant::Numeric(12))),
                ))),
            )),
        )),
    ));
    assert_eq!(expected, actual);
}

#[test]
fn test_evaluated_module_expression_is_module_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Module(
        "foo".to_string(),
        maplit::hashmap! {
           "bar".to_string() => Value::Closure(Closure::Empty(Expression::Constant(Constant::Numeric(42)), Environment::new()))
        },
    );
    let actual = evaluator.evaluate(&Expression::Module(
        "foo".to_string(),
        vec![Expression::Identified(
            "bar".to_string(),
            Box::new(Expression::Lambda(Lambda::Empty(Box::new(
                Expression::Constant(Constant::Numeric(42)),
            )))),
        )],
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_identified_expression_is_identified_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Identified(
        "foo".to_string(),
        Box::new(Value::Closure(Closure::Parametrized(
            "x".to_string(),
            Expression::Constant(Constant::Numeric(42)),
            Environment::new(),
        ))),
    );
    let actual = evaluator.evaluate(&Expression::Identified(
        "foo".to_string(),
        Box::new(Expression::Lambda(Lambda::Parametrized(
            "x".to_string(),
            Box::new(Expression::Constant(Constant::Numeric(42))),
        ))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}
