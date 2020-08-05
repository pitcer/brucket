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
use crate::parser::Visibility;

type TestResult = Result<(), String>;

#[test]
fn test_evaluated_constant_expression_is_numeric_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(42);
    let actual = evaluator.evaluate(&Expression::ConstantValue(ConstantValue::Numeric(42)))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_boolean_expression_is_boolean_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Boolean(true);
    let actual = evaluator.evaluate(&Expression::ConstantValue(ConstantValue::Boolean(true)))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_string_expression_is_textual_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Textual("foobar".to_string());
    let actual = evaluator.evaluate(&Expression::ConstantValue(ConstantValue::String(
        "foobar".to_string(),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_unit_expression_is_unit_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Unit;
    let actual = evaluator.evaluate(&Expression::ConstantValue(ConstantValue::Unit))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_null_expression_is_null_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Null;
    let actual = evaluator.evaluate(&Expression::ConstantValue(ConstantValue::Null))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_let_expression_variable_has_correct_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(42);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
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
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(40))),
        Box::new(Expression::Let(
            "y".to_string(),
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(2))),
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
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(40))),
        Box::new(Expression::Let(
            "x".to_string(),
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(2))),
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
        Box::new(Expression::ConstantValue(ConstantValue::Boolean(true))),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(24))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_without_parameters_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        Vec::new(),
        Box::from(Expression::Identifier("x".to_string())),
        environment!("x" => Value::Numeric(42)),
    ));
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Box::new(Expression::Lambda(Lambda::new(
            Vec::new(),
            Box::new(Expression::Identifier("x".to_string())),
            maplit::hashset!("x".to_string()),
        ))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_parameter_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![Parameter::Unary("y".to_string())],
        Box::from(Expression::Identifier("y".to_string())),
        Environment::new(),
    ));
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Box::new(Expression::Lambda(Lambda::new(
            vec![Parameter::Unary("y".to_string())],
            Box::new(Expression::Identifier("y".to_string())),
            HashSet::new(),
        ))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_parameters_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![
            Parameter::Unary("y".to_string()),
            Parameter::Unary("z".to_string()),
            Parameter::Unary("a".to_string()),
        ],
        Box::from(Expression::Identifier("y".to_string())),
        Environment::new(),
    ));
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Box::new(Expression::Lambda(Lambda::new(
            vec![
                Parameter::Unary("y".to_string()),
                Parameter::Unary("z".to_string()),
                Parameter::Unary("a".to_string()),
            ],
            Box::new(Expression::Identifier("y".to_string())),
            HashSet::new(),
        ))),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_without_parameters_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(42);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Box::new(Expression::Application(
            Box::new(Expression::Lambda(Lambda::new(
                Vec::new(),
                Box::new(Expression::Identifier("x".to_string())),
                maplit::hashset!("x".to_string()),
            ))),
            Vec::new(),
        )),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_with_parameter_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(24);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Box::new(Expression::Application(
            Box::new(Expression::Lambda(Lambda::new(
                vec![Parameter::Unary("y".to_string())],
                Box::new(Expression::Identifier("y".to_string())),
                HashSet::new(),
            ))),
            vec![Expression::ConstantValue(ConstantValue::Numeric(24))],
        )),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_with_parameters_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(24);
    let actual = evaluator.evaluate(&Expression::Let(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Box::new(Expression::Application(
            Box::new(Expression::Lambda(Lambda::new(
                vec![
                    Parameter::Unary("y".to_string()),
                    Parameter::Unary("z".to_string()),
                    Parameter::Unary("a".to_string()),
                ],
                Box::new(Expression::Identifier("y".to_string())),
                HashSet::new(),
            ))),
            vec![
                Expression::ConstantValue(ConstantValue::Numeric(24)),
                Expression::ConstantValue(ConstantValue::Numeric(4)),
                Expression::ConstantValue(ConstantValue::Numeric(2)),
            ],
        )),
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
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Box::new(Expression::Let(
            "y".to_string(),
            Box::new(Expression::Lambda(Lambda::new(
                vec![Parameter::Unary("a".to_string())],
                Box::new(Expression::Identifier("z".to_string())),
                maplit::hashset!("z".to_string()),
            ))),
            Box::new(Expression::Let(
                "z".to_string(),
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(24))),
                Box::new(Expression::Application(
                    Box::new(Expression::Identifier("y".to_string())),
                    vec![Expression::ConstantValue(ConstantValue::Numeric(12))],
                )),
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
        environment! {
            "bar" => Value::Closure(Closure::new(
                Vec::new(),
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
                Environment::new(),
            ))
        },
    );
    let actual = evaluator.evaluate(&Expression::Module {
        identifier: "foo".to_string(),
        functions: vec![Expression::Function(
            Visibility::Public,
            "bar".to_string(),
            Lambda::new(
                Vec::new(),
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
                HashSet::new(),
            ),
        )],
        constants: Vec::new(),
    })?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_function_expression_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![Parameter::Unary("x".to_string())],
        Box::from(Expression::ConstantValue(ConstantValue::Numeric(42))),
        Environment::new(),
    ));
    let actual = evaluator.evaluate(&Expression::Function(
        Visibility::Private,
        "foo".to_string(),
        Lambda::new(
            vec![Parameter::Unary("x".to_string())],
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
            HashSet::new(),
        ),
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_and_expression_is_false_if_false_argument_exists() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Boolean(false);
    let actual = evaluator.evaluate(&Expression::And(vec![
        Expression::ConstantValue(ConstantValue::Boolean(true)),
        Expression::ConstantValue(ConstantValue::Boolean(true)),
        Expression::ConstantValue(ConstantValue::Boolean(false)),
        Expression::ConstantValue(ConstantValue::Boolean(true)),
    ]))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_and_expression_is_true_if_every_argument_is_true() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Boolean(true);
    let actual = evaluator.evaluate(&Expression::And(vec![
        Expression::ConstantValue(ConstantValue::Boolean(true)),
        Expression::ConstantValue(ConstantValue::Boolean(true)),
        Expression::ConstantValue(ConstantValue::Boolean(true)),
        Expression::ConstantValue(ConstantValue::Boolean(true)),
    ]))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_or_expression_is_true_if_true_argument_exists() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Boolean(true);
    let actual = evaluator.evaluate(&Expression::Or(vec![
        Expression::ConstantValue(ConstantValue::Boolean(false)),
        Expression::ConstantValue(ConstantValue::Boolean(false)),
        Expression::ConstantValue(ConstantValue::Boolean(true)),
        Expression::ConstantValue(ConstantValue::Boolean(false)),
    ]))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_or_expression_is_false_if_every_argument_is_false() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Boolean(false);
    let actual = evaluator.evaluate(&Expression::Or(vec![
        Expression::ConstantValue(ConstantValue::Boolean(false)),
        Expression::ConstantValue(ConstantValue::Boolean(false)),
        Expression::ConstantValue(ConstantValue::Boolean(false)),
        Expression::ConstantValue(ConstantValue::Boolean(false)),
    ]))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_variadic_parameter_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![
            Parameter::Unary("x".to_string()),
            Parameter::Unary("y".to_string()),
            Parameter::Variadic("z".to_string()),
        ],
        Box::from(Expression::Identifier("x".to_string())),
        Environment::new(),
    ));
    let actual = evaluator.evaluate(&Expression::Lambda(Lambda::new(
        vec![
            Parameter::Unary("x".to_string()),
            Parameter::Unary("y".to_string()),
            Parameter::Variadic("z".to_string()),
        ],
        Box::new(Expression::Identifier("x".to_string())),
        HashSet::new(),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_with_variadic_parameter_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Pair(
        Box::new(Value::Numeric(3)),
        Box::new(Value::Pair(
            Box::new(Value::Numeric(4)),
            Box::new(Value::Pair(
                Box::new(Value::Numeric(5)),
                Box::new(Value::Null),
            )),
        )),
    );
    let actual = evaluator.evaluate(&Expression::Application(
        Box::new(Expression::Lambda(Lambda::new(
            vec![
                Parameter::Unary("x".to_string()),
                Parameter::Unary("y".to_string()),
                Parameter::Variadic("z".to_string()),
            ],
            Box::new(Expression::Identifier("z".to_string())),
            HashSet::new(),
        ))),
        vec![
            Expression::ConstantValue(ConstantValue::Numeric(1)),
            Expression::ConstantValue(ConstantValue::Numeric(2)),
            Expression::ConstantValue(ConstantValue::Numeric(3)),
            Expression::ConstantValue(ConstantValue::Numeric(4)),
            Expression::ConstantValue(ConstantValue::Numeric(5)),
        ],
    ))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_private_members_are_not_included_in_module_environment() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Module(
        "foo".to_string(),
        environment! {
            "bar" => Value::Closure(Closure::new(
                Vec::new(),
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
                Environment::new(),
            )),
            "baar" => Value::Numeric(42)
        },
    );
    let actual = evaluator.evaluate(&Expression::Module {
        identifier: "foo".to_string(),
        functions: vec![
            Expression::Function(
                Visibility::Public,
                "bar".to_string(),
                Lambda::new(
                    Vec::new(),
                    Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
                    HashSet::new(),
                ),
            ),
            Expression::Function(
                Visibility::Private,
                "fooo".to_string(),
                Lambda::new(
                    Vec::new(),
                    Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
                    HashSet::new(),
                ),
            ),
        ],
        constants: vec![
            Expression::Constant(
                Visibility::Public,
                "baar".to_string(),
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
            ),
            Expression::Constant(
                Visibility::Private,
                "barr".to_string(),
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(42))),
            ),
        ],
    })?;
    assert_eq!(expected, actual);
    Ok(())
}
