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

use brucket_ast::ast::ast_type::Type;
use brucket_ast::ast::constant_value::{Boolean, ConstantValue};
use brucket_ast::ast::lambda::Parameter;
use brucket_ast::ast::{Application, Constant, Identifier, If, Let, NodeId, Visibility};

use super::*;
use brucket_analyzer::variables_analyzer::NodeVariables;
use brucket_ast::ast::function::Function;

type TestResult = Result<(), ValueError>;

#[test]
fn test_evaluated_constant_expression_is_numeric_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate(&Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Numeric(Number::Integer("42".to_string())),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_boolean_expression_is_boolean_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Boolean(true);
    let actual = evaluator.evaluate(&Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Boolean(Boolean::True),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_string_expression_is_textual_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Textual("foobar".to_string());
    let actual = evaluator.evaluate(&Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::String("foobar".to_string()),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_unit_expression_is_unit_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Unit;
    let actual = evaluator.evaluate(&Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Unit,
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_null_expression_is_null_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Null;
    let actual = evaluator.evaluate(&Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Null,
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_let_expression_variable_has_correct_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate(&Node::Let(Let::new(
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
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_not_overwritten_by_second_with_different_name() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(40));
    let actual = evaluator.evaluate(&Node::Let(Let::new(
        NodeId(0),
        "x".to_string(),
        Type::Any,
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("40".to_string())),
        ))),
        Box::new(Node::Let(Let::new(
            NodeId(0),
            "y".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("2".to_string())),
            ))),
            Box::new(Node::Identifier(Identifier::new(
                NodeId(0),
                Path::Simple("x".to_string()),
            ))),
        ))),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_overwritten_by_second_with_the_same_name() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(2));
    let actual = evaluator.evaluate(&Node::Let(Let::new(
        NodeId(0),
        "x".to_string(),
        Type::Any,
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("40".to_string())),
        ))),
        Box::new(Node::Let(Let::new(
            NodeId(0),
            "x".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("2".to_string())),
            ))),
            Box::new(Node::Identifier(Identifier::new(
                NodeId(0),
                Path::Simple("x".to_string()),
            ))),
        ))),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_if_expression_has_correct_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate(&Node::If(If::new(
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
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_without_parameters_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        Vec::new(),
        Box::from(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_string()),
        ))),
        environment!("x" => Value::Numeric(Numeric::Integer(42))),
    ));
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::new(
                HashSet::default(),
                maplit::hashset![Path::Simple("x".to_owned())]
            )
        }),
        &Node::Let(Let::new(
            NodeId(0),
            "x".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Box::new(Node::Lambda(Lambda::new(
                NodeId(1),
                Vec::new(),
                Type::Any,
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("x".to_string()),
                ))),
                // maplit::hashset!("x".to_string()),
            ))),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_parameter_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![Parameter::new("y".to_string(), Type::Any, Arity::Unary)],
        Box::from(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("y".to_string()),
        ))),
        Environment::new(),
    ));
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &Node::Let(Let::new(
            NodeId(0),
            "x".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Box::new(Node::Lambda(Lambda::new(
                NodeId(1),
                vec![Parameter::new("y".to_string(), Type::Any, Arity::Unary)],
                Type::Any,
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("y".to_string()),
                ))),
                // HashSet::new(),
            ))),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_parameters_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![
            Parameter::new("y".to_string(), Type::Any, Arity::Unary),
            Parameter::new("z".to_string(), Type::Any, Arity::Unary),
            Parameter::new("a".to_string(), Type::Any, Arity::Unary),
        ],
        Box::from(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("y".to_string()),
        ))),
        Environment::new(),
    ));
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &Node::Let(Let::new(
            NodeId(0),
            "x".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Box::new(Node::Lambda(Lambda::new(
                NodeId(1),
                vec![
                    Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                    Parameter::new("z".to_string(), Type::Any, Arity::Unary),
                    Parameter::new("a".to_string(), Type::Any, Arity::Unary),
                ],
                Type::Any,
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("y".to_string()),
                ))),
                // HashSet::new(),
            ))),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_without_parameters_is_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::new(
                HashSet::default(),
                maplit::hashset![Path::Simple("x".to_owned())]
            )
        }),
        &Node::Let(Let::new(
            NodeId(0),
            "x".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Lambda(Lambda::new(
                    NodeId(1),
                    Vec::new(),
                    Type::Any,
                    Box::new(Node::Identifier(Identifier::new(
                        NodeId(0),
                        Path::Simple("x".to_string()),
                    ))),
                    // maplit::hashset!("x".to_string()),
                ))),
                Vec::new(),
            ))),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_with_parameter_is_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(24));
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &Node::Let(Let::new(
            NodeId(0),
            "x".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Lambda(Lambda::new(
                    NodeId(1),
                    vec![Parameter::new("y".to_string(), Type::Any, Arity::Unary)],
                    Type::Any,
                    Box::new(Node::Identifier(Identifier::new(
                        NodeId(0),
                        Path::Simple("y".to_string()),
                    ))),
                ))),
                vec![Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("24".to_string())),
                ))],
            ))),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_with_parameters_is_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(24));
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &Node::Let(Let::new(
            NodeId(0),
            "x".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Lambda(Lambda::new(
                    NodeId(1),
                    vec![
                        Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                        Parameter::new("z".to_string(), Type::Any, Arity::Unary),
                        Parameter::new("a".to_string(), Type::Any, Arity::Unary),
                    ],
                    Type::Any,
                    Box::new(Node::Identifier(Identifier::new(
                        NodeId(0),
                        Path::Simple("y".to_string()),
                    ))),
                    // HashSet::new(),
                ))),
                vec![
                    Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("24".to_string())),
                    )),
                    Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("4".to_string())),
                    )),
                    Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("2".to_string())),
                    )),
                ],
            ))),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_closure_does_not_have_access_to_variable_outside_its_environment() {
    let mut evaluator = Evaluator::default();
    let expected = Err(Cow::from("Undefined variable: z"));
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::new(
                HashSet::default(),
                maplit::hashset![Path::Simple("z".to_owned())]
            )
        }),
        &Node::Let(Let::new(
            NodeId(0),
            "x".to_string(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Box::new(Node::Let(Let::new(
                NodeId(0),
                "y".to_string(),
                Type::Any,
                Box::new(Node::Lambda(Lambda::new(
                    NodeId(1),
                    vec![Parameter::new("a".to_string(), Type::Any, Arity::Unary)],
                    Type::Any,
                    Box::new(Node::Identifier(Identifier::new(
                        NodeId(0),
                        Path::Simple("z".to_string()),
                    ))),
                    // maplit::hashset!("z".to_string()),
                ))),
                Box::new(Node::Let(Let::new(
                    NodeId(0),
                    "z".to_string(),
                    Type::Any,
                    Box::new(Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("24".to_string())),
                    ))),
                    Box::new(Node::Application(Application::new(
                        NodeId(0),
                        Box::new(Node::Identifier(Identifier::new(
                            NodeId(0),
                            Path::Simple("y".to_string()),
                        ))),
                        vec![Node::ConstantValue(ConstantValue::new(
                            NodeId(0),
                            ConstantVariant::Numeric(Number::Integer("12".to_string())),
                        ))],
                    ))),
                ))),
            ))),
        )),
    );
    assert_eq!(expected, actual);
}

#[test]
fn test_evaluated_module_expression_is_module_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Module(
        false,
        "foo".to_string(),
        environment! {
            "bar" => Value::FunctionClosure(
                ApplicationStrategy::Eager,
                Closure::new(
                    Vec::new(),
                    Box::new(Node::ConstantValue(ConstantValue::new(NodeId(0),ConstantVariant::Numeric(Number::Integer("42".to_string()))))),
                    Environment::new(),
            ))
        },
    );
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &Node::Module(Module::new(
            NodeId(0),
            false,
            "foo".to_string(),
            vec![Function::new(
                NodeId(0),
                Visibility::Public,
                ApplicationStrategy::Eager,
                "bar".to_string(),
                Lambda::new(
                    NodeId(1),
                    Vec::new(),
                    Type::Any,
                    Box::new(Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("42".to_string())),
                    ))),
                    // HashSet::new(),
                ),
            )],
            Vec::new(),
            Vec::new(),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_function_expression_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::FunctionClosure(
        ApplicationStrategy::Eager,
        Closure::new(
            vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
            Box::from(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Environment::new(),
        ),
    );
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &Node::Function(Function::new(
            NodeId(0),
            Visibility::Private,
            ApplicationStrategy::Eager,
            "foo".to_string(),
            Lambda::new(
                NodeId(1),
                vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
                Type::Any,
                Box::new(
                    Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("42".to_string())),
                    )),
                    // HashSet::new(),
                ),
            ),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_variadic_parameter_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        vec![
            Parameter::new("x".to_string(), Type::Any, Arity::Unary),
            Parameter::new("y".to_string(), Type::Any, Arity::Unary),
            Parameter::new("z".to_string(), Type::Any, Arity::Variadic),
        ],
        Box::from(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_string()),
        ))),
        Environment::new(),
    ));
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &Node::Lambda(Lambda::new(
            NodeId(1),
            vec![
                Parameter::new("x".to_string(), Type::Any, Arity::Unary),
                Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                Parameter::new("z".to_string(), Type::Any, Arity::Variadic),
            ],
            Type::Any,
            Box::new(Node::Identifier(Identifier::new(
                NodeId(0),
                Path::Simple("x".to_string()),
            ))),
            // HashSet::new(),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_with_variadic_parameter_is_value() -> TestResult {
    let mut evaluator = Evaluator::default();
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
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &Node::Application(Application::new(
            NodeId(0),
            Box::new(Node::Lambda(Lambda::new(
                NodeId(1),
                vec![
                    Parameter::new("x".to_string(), Type::Any, Arity::Unary),
                    Parameter::new("y".to_string(), Type::Any, Arity::Unary),
                    Parameter::new("z".to_string(), Type::Any, Arity::Variadic),
                ],
                Type::Any,
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("z".to_string()),
                ))),
                // HashSet::new(),
            ))),
            vec![
                Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("1".to_string())),
                )),
                Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("2".to_string())),
                )),
                Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("3".to_string())),
                )),
                Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("4".to_string())),
                )),
                Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("5".to_string())),
                )),
            ],
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_private_members_are_not_included_in_module_environment() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Module(
        false,
        "foo".to_string(),
        environment! {
            "bar" => Value::FunctionClosure(
                ApplicationStrategy::Eager,
                Closure::new(
                    Vec::new(),
                    Box::new(Node::ConstantValue(ConstantValue::new(NodeId(0),ConstantVariant::Numeric(Number::Integer("42".to_string()))))),
                    Environment::new(),
            )),
            "baar" => Value::Numeric(Numeric::Integer(42))
        },
    );
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default(),
            NodeId(2) => NodeVariables::default()
        }),
        &Node::Module(Module::new(
            NodeId(0),
            false,
            "foo".to_string(),
            vec![
                Function::new(
                    NodeId(0),
                    Visibility::Public,
                    ApplicationStrategy::Eager,
                    "bar".to_string(),
                    Lambda::new(
                        NodeId(1),
                        Vec::new(),
                        Type::Any,
                        Box::new(Node::ConstantValue(ConstantValue::new(
                            NodeId(0),
                            ConstantVariant::Numeric(Number::Integer("42".to_string())),
                        ))),
                        // HashSet::new(),
                    ),
                ),
                Function::new(
                    NodeId(0),
                    Visibility::Private,
                    ApplicationStrategy::Eager,
                    "fooo".to_string(),
                    Lambda::new(
                        NodeId(2),
                        Vec::new(),
                        Type::Any,
                        Box::new(Node::ConstantValue(ConstantValue::new(
                            NodeId(0),
                            ConstantVariant::Numeric(Number::Integer("42".to_string())),
                        ))),
                        // HashSet::new(),
                    ),
                ),
            ],
            vec![],
            vec![
                Constant::new(
                    NodeId(0),
                    Visibility::Public,
                    "baar".to_string(),
                    Box::new(Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("42".to_string())),
                    ))),
                ),
                Constant::new(
                    NodeId(0),
                    Visibility::Private,
                    "barr".to_string(),
                    Box::new(Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("42".to_string())),
                    ))),
                ),
            ],
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lazy_function_expression_is_lazy_function_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::FunctionClosure(
        ApplicationStrategy::Lazy,
        Closure::new(
            Vec::new(),
            Box::from(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))),
            Environment::new(),
        ),
    );
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default(),
        }),
        &Node::Function(Function::new(
            NodeId(0),
            Visibility::Private,
            ApplicationStrategy::Lazy,
            "foo".to_string(),
            Lambda::new(
                NodeId(1),
                Vec::new(),
                Type::Any,
                Box::new(
                    Node::ConstantValue(ConstantValue::new(
                        NodeId(0),
                        ConstantVariant::Numeric(Number::Integer("42".to_string())),
                    )),
                    // HashSet::new(),
                ),
            ),
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lazy_identity_function_application_expression_is_thunk_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Thunk(
        Box::from(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_string())),
        ))),
        Environment::new(),
    );
    let actual = evaluator.evaluate_with_variables(
        Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default(),
        }),
        &Node::Application(Application::new(
            NodeId(0),
            Box::new(Node::Function(Function::new(
                NodeId(0),
                Visibility::Private,
                ApplicationStrategy::Lazy,
                "foo".to_string(),
                Lambda::new(
                    NodeId(1),
                    vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
                    Type::Any,
                    Box::new(
                        Node::Identifier(Identifier::new(NodeId(0), Path::Simple("x".to_string()))),
                        // HashSet::new(),
                    ),
                ),
            ))),
            vec![Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_string())),
            ))],
        )),
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_static_module_expression_is_static_module_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Module(true, "foo".to_string(), Environment::new());
    let actual = evaluator.evaluate(&Node::Module(Module::new(
        NodeId(0),
        true,
        "foo".to_string(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
    )))?;
    assert_eq!(expected, actual);
    Ok(())
}
