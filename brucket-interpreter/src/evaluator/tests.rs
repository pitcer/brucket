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

use brucket_analyzer::variables_analyzer::{NodeVariables, Variables};
use brucket_ast::NodeId;
use brucket_quote::brucket;

use super::*;

type TestResult = Result<(), ValueError>;

#[test]
fn test_evaluated_constant_expression_is_numeric_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate_with_default_state(&brucket!(42))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_boolean_expression_is_boolean_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Boolean(true);
    let actual = evaluator.evaluate_with_default_state(&brucket!(true))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_string_expression_is_textual_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Textual("foobar".to_string());
    let actual = evaluator.evaluate_with_default_state(&brucket!("foobar"))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_unit_expression_is_unit_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Unit;
    let actual = evaluator.evaluate_with_default_state(&brucket!(()))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_null_expression_is_null_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Null;
    let actual = evaluator.evaluate_with_default_state(&brucket!(null))?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_let_expression_variable_has_correct_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate_with_default_state(&brucket! {
        (let x 42
            x)
    })?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_not_overwritten_by_second_with_different_name() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(40));
    let actual = evaluator.evaluate_with_default_state(&brucket! {
        (let x 40
            (let y 2
                x))
    })?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_first_variable_is_overwritten_by_second_with_the_same_name() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(2));
    let actual = evaluator.evaluate_with_default_state(&brucket! {
        (let x 40
            (let x 2
                x))
    })?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_if_expression_has_correct_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate_with_default_state(&brucket! {
        (if true 42 24)
    })?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_without_parameters_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        Vec::new(),
        Box::new(brucket!(x)),
        environment!("x" => Value::Numeric(Numeric::Integer(42))),
    ));
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::new(
                HashMap::default(),
                maplit::hashset!["x".to_owned()],
                maplit::hashset!["x".to_owned()]
            )
        }),
        &brucket! {
            (let x 42
                (1: lambda [] -> any x))
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_parameter_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        brucket!(@parameters [y]),
        Box::new(brucket!(y)),
        Environment::default(),
    ));
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &brucket! {
            (let x 42
                (1: lambda [y] -> any y))
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_parameters_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        brucket!(@parameters [y z a]),
        Box::new(brucket!(y)),
        Environment::default(),
    ));
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &brucket! {
            (let x 42
                (1: lambda [y z a] -> any y))
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_without_parameters_is_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::new(
                HashMap::default(),
                maplit::hashset!["x".to_owned()],
                maplit::hashset!["x".to_owned()]
            )
        }),
        &brucket! {
            (let x 42
                ((1: lambda [] -> any x)))
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_with_parameter_is_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(24));
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &brucket! {
            (let x 42
                ((1: lambda [y] -> any y) 24))
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_application_on_lambda_expression_with_parameters_is_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Numeric(Numeric::Integer(24));
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &brucket! {
            (let x 42
                ((1: lambda [y z a] -> any y) 24 4 2))
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_closure_does_not_have_access_to_variable_outside_its_environment() {
    let mut evaluator = Evaluator::default();
    let expected = Err(Cow::from("Undefined variable: z"));
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::new(
                HashMap::default(),
                maplit::hashset!["z".to_owned()],
                maplit::hashset!["z".to_owned()]
            )
        }),
        &brucket! {
            (let x 42
                (let y (1: lambda [a] -> any z)
                    (let z 24
                        (y 12))))
        },
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
                    Box::new(brucket!(42)),
                    Environment::default(),
            ))
        },
    );
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &brucket! {
            @node Module
            (module foo ((0: public eager function bar [] -> any 1: 42)) () ())
        },
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
            brucket!(@parameters [x]),
            Box::new(brucket!(42)),
            Environment::default(),
        ),
    );
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &brucket! {
            @node Function
            (0: public eager function foo [x] -> any 1: 42)
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lambda_expression_with_variadic_parameter_is_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Closure(Closure::new(
        brucket!(@parameters [(x: any) (y: any) (z: any...)]),
        Box::new(brucket!(x)),
        Environment::default(),
    ));
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &brucket! {
            (1: lambda [(x: any) (y: any) (z: any...)] -> any x)
        },
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
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default()
        }),
        &brucket! {
            ((1: lambda [(x: any) (y: any) (z: any...)] -> any z) 1 2 3 4 5)
        },
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
                    Box::new(brucket!(42)),
                    Environment::default(),
            )),
            "baar" => Value::Numeric(Numeric::Integer(42))
        },
    );
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default(),
            NodeId(2) => NodeVariables::default()
        }),
        &brucket! {
            @node Module
            (module foo
                (
                    (0: public eager function bar [] -> any 1: 42)
                    (0: private eager function fooo [] -> any 2: 42)
                )
                ()
                (
                    (0: public constant baar 42)
                    (0: private constant barr 42)
                )
            )
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lazy_function_expression_is_lazy_function_closure_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::FunctionClosure(
        ApplicationStrategy::Lazy,
        Closure::new(Vec::new(), Box::new(brucket!(42)), Environment::default()),
    );
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default(),
        }),
        &brucket! {
            @node Function
            (0: private lazy function foo [] -> any 1: 42)
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_lazy_identity_function_application_expression_is_thunk_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Thunk(Box::new(brucket!(42)), Environment::default());
    let actual = evaluator.evaluate_with_variables(
        &Variables(maplit::hashmap! {
            NodeId(1) => NodeVariables::default(),
        }),
        &brucket! {
            ((@node Function (0: private lazy function foo [x] -> any 1: x)) 42)
        },
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_evaluated_static_module_expression_is_static_module_value() -> TestResult {
    let mut evaluator = Evaluator::default();
    let expected = Value::Module(true, "foo".to_string(), Environment::default());
    let actual = evaluator.evaluate_with_default_state(&brucket! {
        @node Module
        (0: static module foo () () ())
    })?;
    assert_eq!(expected, actual);
    Ok(())
}
