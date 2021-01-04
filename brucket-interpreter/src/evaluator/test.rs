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

use brucket_ast::ast::{Boolean, Parameter, Type, Visibility};

use super::*;

type TestResult = Result<(), ValueError>;

#[test]
fn test_evaluated_constant_expression_is_numeric_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate(&Expression::ConstantValue(ConstantValue::Numeric(
        Number::Integer("42".to_string()),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_boolean_expression_is_boolean_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Boolean(true);
    let actual = evaluator.evaluate(&Expression::ConstantValue(ConstantValue::Boolean(
        Boolean::True,
    )))?;
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
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate(&Expression::Let(Let {
        name: "x".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        then: Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_not_overwritten_by_second_with_different_name() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(40));
    let actual = evaluator.evaluate(&Expression::Let(Let::new(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("40".to_string()),
        ))),
        Box::new(Expression::Let(Let::new(
            "y".to_string(),
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("2".to_string()),
            ))),
            Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
        ))),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_overwritten_by_second_with_the_same_name() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(2));
    let actual = evaluator.evaluate(&Expression::Let(Let::new(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("40".to_string()),
        ))),
        Box::new(Expression::Let(Let::new(
            "x".to_string(),
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("2".to_string()),
            ))),
            Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
        ))),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_if_expression_has_correct_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate(&Expression::If(If::new(
        Box::new(Expression::ConstantValue(ConstantValue::Boolean(
            Boolean::True,
        ))),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("24".to_string()),
        ))),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_without_parameters_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        Vec::new(),
        Box::from(Expression::Identifier(Path::Simple("x".to_string()))),
        environment!("x" => Value::Numeric(Numeric::Integer(42))),
    ));
    let actual = evaluator.evaluate(&Expression::Let(Let {
        name: "x".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        then: Box::new(Expression::Lambda(Lambda::new(
            Vec::new(),
            Type::Any,
            Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
            maplit::hashset!("x".to_string()),
        ))),
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_parameter_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![Parameter::new("y".to_string(), Type::Any, Arity::Unary)],
        Box::from(Expression::Identifier(Path::Simple("y".to_string()))),
        Environment::new(),
    ));
    let actual = evaluator.evaluate(&Expression::Let(Let {
        name: "x".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        then: Box::new(Expression::Lambda(Lambda::new(
            vec![Parameter::new("y".to_string(), Type::Any, Arity::Unary)],
            Type::Any,
            Box::new(Expression::Identifier(Path::Simple("y".to_string()))),
            HashSet::new(),
        ))),
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_parameters_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![
            Parameter::new("y".to_string(), Type::Any, Arity::Unary),
            Parameter::new("z".to_string(), Type::Any, Arity::Unary),
            Parameter::new("a".to_string(), Type::Any, Arity::Unary),
        ],
        Box::from(Expression::Identifier(Path::Simple("y".to_string()))),
        Environment::new(),
    ));
    let actual = evaluator.evaluate(&Expression::Let(Let {
        name: "x".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        then: Box::new(Expression::Lambda(Lambda::new(
            vec![
                Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                Parameter::new("z".to_string(), Type::Any, Arity::Unary),
                Parameter::new("a".to_string(), Type::Any, Arity::Unary),
            ],
            Type::Any,
            Box::new(Expression::Identifier(Path::Simple("y".to_string()))),
            HashSet::new(),
        ))),
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_without_parameters_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate(&Expression::Let(Let {
        name: "x".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        then: Box::new(Expression::Application(Application {
            identifier: Box::new(Expression::Lambda(Lambda::new(
                Vec::new(),
                Type::Any,
                Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
                maplit::hashset!("x".to_string()),
            ))),
            arguments: Vec::new(),
        })),
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_with_parameter_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(24));
    let actual = evaluator.evaluate(&Expression::Let(Let {
        name: "x".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        then: Box::new(Expression::Application(Application {
            identifier: Box::new(Expression::Lambda(Lambda::new(
                vec![Parameter::new("y".to_string(), Type::Any, Arity::Unary)],
                Type::Any,
                Box::new(Expression::Identifier(Path::Simple("y".to_string()))),
                HashSet::new(),
            ))),
            arguments: vec![Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("24".to_string()),
            ))],
        })),
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_with_parameters_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(24));
    let actual = evaluator.evaluate(&Expression::Let(Let {
        name: "x".to_string(),
        value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        then: Box::new(Expression::Application(Application {
            identifier: Box::new(Expression::Lambda(Lambda::new(
                vec![
                    Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                    Parameter::new("z".to_string(), Type::Any, Arity::Unary),
                    Parameter::new("a".to_string(), Type::Any, Arity::Unary),
                ],
                Type::Any,
                Box::new(Expression::Identifier(Path::Simple("y".to_string()))),
                HashSet::new(),
            ))),
            arguments: vec![
                Expression::ConstantValue(ConstantValue::Numeric(Number::Integer(
                    "24".to_string(),
                ))),
                Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("4".to_string()))),
                Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("2".to_string()))),
            ],
        })),
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_closure_does_not_have_access_to_variable_outside_its_environment() {
    let evaluator = Evaluator::default();
    let expected = Err(Cow::from("Undefined variable: z"));
    let actual = evaluator.evaluate(&Expression::Let(Let::new(
        "x".to_string(),
        Box::new(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        Box::new(Expression::Let(Let::new(
            "y".to_string(),
            Box::new(Expression::Lambda(Lambda::new(
                vec![Parameter::new("a".to_string(), Type::Any, Arity::Unary)],
                Type::Any,
                Box::new(Expression::Identifier(Path::Simple("z".to_string()))),
                maplit::hashset!("z".to_string()),
            ))),
            Box::new(Expression::Let(Let::new(
                "z".to_string(),
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                    Number::Integer("24".to_string()),
                ))),
                Box::new(Expression::Application(Application {
                    identifier: Box::new(Expression::Identifier(Path::Simple("y".to_string()))),
                    arguments: vec![Expression::ConstantValue(ConstantValue::Numeric(
                        Number::Integer("12".to_string()),
                    ))],
                })),
            ))),
        ))),
    )));
    assert_eq!(expected, actual);
}

#[test]
fn test_evaluated_module_expression_is_module_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Module(
        false,
        "foo".to_string(),
        environment! {
            "bar" => Value::FunctionClosure(
                ApplicationStrategy::Eager,
                Closure::new(
                    Vec::new(),
                    Box::new(Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("42".to_string())))),
                    Environment::new(),
            ))
        },
    );
    let actual = evaluator.evaluate(&Expression::Module(Module::new(
        false,
        "foo".to_string(),
        vec![Expression::Function(Function {
            visibility: Visibility::Public,
            application_strategy: ApplicationStrategy::Eager,
            name: "bar".to_string(),
            body: Lambda::new(
                Vec::new(),
                Type::Any,
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                    Number::Integer("42".to_string()),
                ))),
                HashSet::new(),
            ),
        })],
        Vec::new(),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_function_expression_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::FunctionClosure(
        ApplicationStrategy::Eager,
        Closure::new(
            vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
            Box::from(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("42".to_string()),
            ))),
            Environment::new(),
        ),
    );
    let actual = evaluator.evaluate(&Expression::Function(Function {
        visibility: Visibility::Private,
        application_strategy: ApplicationStrategy::Eager,
        name: "foo".to_string(),
        body: Lambda::new(
            vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
            Type::Any,
            Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("42".to_string()),
            ))),
            HashSet::new(),
        ),
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_variadic_parameter_is_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![
            Parameter::new("x".to_string(), Type::Any, Arity::Unary),
            Parameter::new("y".to_string(), Type::Any, Arity::Unary),
            Parameter::new("z".to_string(), Type::Any, Arity::Variadic),
        ],
        Box::from(Expression::Identifier(Path::Simple("x".to_string()))),
        Environment::new(),
    ));
    let actual = evaluator.evaluate(&Expression::Lambda(Lambda::new(
        vec![
            Parameter::new("x".to_string(), Type::Any, Arity::Unary),
            Parameter::new("y".to_string(), Type::Any, Arity::Unary),
            Parameter::new("z".to_string(), Type::Any, Arity::Variadic),
        ],
        Type::Any,
        Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
        HashSet::new(),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_with_variadic_parameter_is_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Pair(
        Box::new(Value::Numeric(Numeric::Integer(3))),
        Box::new(Value::Pair(
            Box::new(Value::Numeric(Numeric::Integer(4))),
            Box::new(Value::Pair(
                Box::new(Value::Numeric(Numeric::Integer(5))),
                Box::new(Value::Null),
            )),
        )),
    );
    let actual = evaluator.evaluate(&Expression::Application(Application {
        identifier: Box::new(Expression::Lambda(Lambda::new(
            vec![
                Parameter::new("x".to_string(), Type::Any, Arity::Unary),
                Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                Parameter::new("z".to_string(), Type::Any, Arity::Variadic),
            ],
            Type::Any,
            Box::new(Expression::Identifier(Path::Simple("z".to_string()))),
            HashSet::new(),
        ))),
        arguments: vec![
            Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("1".to_string()))),
            Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("2".to_string()))),
            Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("3".to_string()))),
            Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("4".to_string()))),
            Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("5".to_string()))),
        ],
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_private_members_are_not_included_in_module_environment() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Module(
        false,
        "foo".to_string(),
        environment! {
            "bar" => Value::FunctionClosure(
                ApplicationStrategy::Eager,
                Closure::new(
                    Vec::new(),
                    Box::new(Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("42".to_string())))),
                    Environment::new(),
            )),
            "baar" => Value::Numeric(Numeric::Integer(42))
        },
    );
    let actual = evaluator.evaluate(&Expression::Module(Module::new(
        false,
        "foo".to_string(),
        vec![
            Expression::Function(Function {
                visibility: Visibility::Public,
                application_strategy: ApplicationStrategy::Eager,
                name: "bar".to_string(),
                body: Lambda::new(
                    Vec::new(),
                    Type::Any,
                    Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                        Number::Integer("42".to_string()),
                    ))),
                    HashSet::new(),
                ),
            }),
            Expression::Function(Function {
                visibility: Visibility::Private,
                application_strategy: ApplicationStrategy::Eager,
                name: "fooo".to_string(),
                body: Lambda::new(
                    Vec::new(),
                    Type::Any,
                    Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                        Number::Integer("42".to_string()),
                    ))),
                    HashSet::new(),
                ),
            }),
        ],
        vec![
            Expression::Constant(Constant {
                visibility: Visibility::Public,
                name: "baar".to_string(),
                value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                    Number::Integer("42".to_string()),
                ))),
            }),
            Expression::Constant(Constant {
                visibility: Visibility::Private,
                name: "barr".to_string(),
                value: Box::new(Expression::ConstantValue(ConstantValue::Numeric(
                    Number::Integer("42".to_string()),
                ))),
            }),
        ],
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lazy_function_expression_is_lazy_function_closure_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::FunctionClosure(
        ApplicationStrategy::Lazy,
        Closure::new(
            Vec::new(),
            Box::from(Expression::ConstantValue(ConstantValue::Numeric(
                Number::Integer("42".to_string()),
            ))),
            Environment::new(),
        ),
    );
    let actual = evaluator.evaluate(&Expression::Function(Function {
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
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lazy_identity_function_application_expression_is_thunk_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Thunk(
        Box::from(Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))),
        Environment::new(),
    );
    let actual = evaluator.evaluate(&Expression::Application(Application {
        identifier: Box::new(Expression::Function(Function {
            visibility: Visibility::Private,
            application_strategy: ApplicationStrategy::Lazy,
            name: "foo".to_string(),
            body: Lambda::new(
                vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
                Type::Any,
                Box::new(Expression::Identifier(Path::Simple("x".to_string()))),
                HashSet::new(),
            ),
        })),
        arguments: vec![Expression::ConstantValue(ConstantValue::Numeric(
            Number::Integer("42".to_string()),
        ))],
    }))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_static_module_expression_is_static_module_value() -> TestResult {
    let evaluator = Evaluator::default();
    let expected = Value::Module(true, "foo".to_string(), Environment::new());
    let actual = evaluator.evaluate(&Expression::Module(Module::new(
        true,
        "foo".to_string(),
        Vec::new(),
        Vec::new(),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}
