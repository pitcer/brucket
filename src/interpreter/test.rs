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
use crate::evaluator::Closure;
use crate::parser::{ConstantValue, Expression, Parameter};
use std::fs::File;
use std::io::Read;

type TestResult = Result<(), String>;

#[test]
fn test_interpret_number() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("42")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_unit_function() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Unit;
    let actual = interpreter.interpret("()")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_simple_arithmetic_expression() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(3);
    let actual = interpreter.interpret("(+ 1 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_arithmetic_expression() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(9);
    let actual = interpreter.interpret("(+ (* 2 3) (- 7 4))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_expression_with_comment() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("# foobar\n(+ 40 2)\n#another comment")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_arithmetic_expression_with_variables() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret(
        r#"
        (let x (+ 40 2)
          (- (let y 2 (* x y))
             (let z 10 (- (+ x z) 10))))
        "#,
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_expression_with_constant_condition() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret(
        r#"
        (let x (+ 20 2)
          (if false
              (- x 2)
              (+ 20 x)))
        "#,
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
#[should_panic(expected = "attempt to divide by zero")]
fn test_division_by_zero_should_panic() {
    let interpreter = create_interpreter();
    let _result = interpreter.interpret("(/ 1 0)");
}

#[test]
fn test_if_expression_is_evaluated_lazily() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(if false (/ 1 0) 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_environment_is_cleared() {
    let interpreter = create_interpreter();
    let expected = Err("Undefined variable: x".to_string());
    let actual = interpreter.interpret("(+ (let x 42 x) x)");
    assert_eq!(expected, actual);
}

#[test]
fn test_interpret_module() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Module(
        "foo".to_string(),
        environment! {
            "bar" => Value::Numeric(1),
            "barfoo" => Value::Closure(Closure::new(
                vec![Parameter::Unary("x".to_string())],
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(2))),
                Environment::new(),
            )),
            "foobar" => Value::Numeric(3),
            "fooo" => Value::Closure(Closure::new(
                vec![Parameter::Unary("x".to_string())],
                Box::new(Expression::ConstantValue(ConstantValue::Numeric(4))),
                Environment::new(),
            ))
        },
    );
    let actual = interpreter.interpret(
        r#"
        (module foo
          (public constant bar 1)
          (public function barfoo |x| 2)
          (private function barr |x| 0)
          (private constant baar 0)
          (public constant foobar 3)
          (public function fooo |x| 4)
        )
        "#,
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_function() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Closure(Closure::new(
        vec![Parameter::Unary("x".to_string())],
        Box::from(Expression::Identifier("x".to_string())),
        Environment::new(),
    ));
    let actual = interpreter.interpret("(function foo |x| x))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_comparison_functions() -> TestResult {
    let interpreter = create_interpreter();
    assert_eq!(Value::Boolean(false), interpreter.interpret("(= 42 24)")?);
    assert_eq!(Value::Boolean(true), interpreter.interpret("(= 42 42)")?);
    assert_eq!(Value::Boolean(false), interpreter.interpret("(> 24 42)")?);
    assert_eq!(Value::Boolean(true), interpreter.interpret("(> 42 24)")?);
    assert_eq!(Value::Boolean(false), interpreter.interpret("(>= 24 42)")?);
    assert_eq!(Value::Boolean(true), interpreter.interpret("(>= 42 24)")?);
    assert_eq!(Value::Boolean(true), interpreter.interpret("(>= 42 42)")?);
    assert_eq!(Value::Boolean(false), interpreter.interpret("(< 42 24)")?);
    assert_eq!(Value::Boolean(true), interpreter.interpret("(< 24 42)")?);
    assert_eq!(Value::Boolean(false), interpreter.interpret("(<= 42 24)")?);
    assert_eq!(Value::Boolean(true), interpreter.interpret("(<= 24 42)")?);
    assert_eq!(Value::Boolean(true), interpreter.interpret("(<= 42 42)")?);
    Ok(())
}

#[test]
fn test_and_is_evaluated_lazily() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Boolean(false);
    let actual = interpreter.interpret("(and true false (/ 1 0))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_or_is_evaluated_lazily() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Boolean(true);
    let actual = interpreter.interpret("(or false true (/ 1 0))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_negate_function_negates_correctly() -> TestResult {
    let interpreter = create_interpreter();
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret("(negate true)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret("(negate false)")?
    );
    Ok(())
}

#[test]
fn test_recursive_lambda() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(0);
    let actual = interpreter.interpret(
        r#"
        (let foo
          (-> |x|
            (if (> x 0)
              (foo (- x 1))
              x))
          (foo 5))
        "#,
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_pair() -> TestResult {
    let interpreter = create_interpreter();
    assert_eq!(
        Value::Numeric(42),
        interpreter.interpret("(pair_first (new_pair 42 24))")?
    );
    assert_eq!(
        Value::Numeric(24),
        interpreter.interpret("(pair_second (new_pair 42 24))")?
    );
    Ok(())
}

#[test]
fn test_is_null() -> TestResult {
    let interpreter = create_interpreter();
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret("(is_null null)")?
    );
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret("(is_null 42)")?
    );
    Ok(())
}

#[test]
fn test_variadic_parameter() -> TestResult {
    let interpreter = create_interpreter();
    assert_eq!(
        Value::Pair(
            Box::new(Value::Numeric(2)),
            Box::new(Value::Pair(
                Box::new(Value::Numeric(3)),
                Box::new(Value::Pair(
                    Box::new(Value::Numeric(4)),
                    Box::new(Value::Null),
                )),
            )),
        ),
        interpreter.interpret("((-> |x xs... y| xs) 1 2 3 4)")?
    );
    assert_eq!(
        Value::Numeric(1),
        interpreter.interpret("((-> |x xs... y| x) 1 2 3 4)")?
    );
    Ok(())
}

#[test]
fn test_call_other_members_in_module() -> TestResult {
    let library = r#"
        (module test
          (constant X 42)

          (function foo |x|
            x)

          (public function foobar ||
            (foo (bar X)))

          (function bar |x|
            x))
        "#;
    let interpreter = Interpreter::with_library(library)?;
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(foobar)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_call_other_members_in_module_including_recursive_function() -> TestResult {
    let library = r#"
        (module test
          (constant X 42)

          (function foo |x|
            (iter x STEP))

          (public function bar ||
            (foo X))

          (function iter |x y|
            (if (internal is_greater y 0)
              (iter x (internal subtract y 1))
              x))

          (constant STEP 3))
        "#;
    let interpreter = Interpreter::with_library(library)?;
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(bar)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_add_with_zero_arguments_returns_zero() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(0);
    let actual = interpreter.interpret("(+)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_add_with_one_argument_returns_that_argument() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(+ 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_add_with_two_arguments_returns_their_sum() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(+ 40 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_add_with_many_arguments_returns_their_sum() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(+ 18 2 9 13)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_zero_arguments_returns_zero() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(0);
    let actual = interpreter.interpret("(-)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_one_argument_returns_that_argument_with_opposite_sign() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(-42);
    let actual = interpreter.interpret("(- 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_two_arguments_returns_their_difference() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(40);
    let actual = interpreter.interpret("(- 42 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_many_arguments_returns_their_difference() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(10);
    let actual = interpreter.interpret("(- 42 10 20 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_zero_arguments_returns_one() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(1);
    let actual = interpreter.interpret("(*)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_one_argument_returns_that_argument() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(* 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_two_arguments_returns_their_product() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(* 21 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_many_arguments_returns_their_product() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(* 7 3 2 1)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_zero_arguments_returns_one() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(1);
    let actual = interpreter.interpret("(/)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_one_argument_returns_inverse_of_that_argument() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(1 / 42);
    let actual = interpreter.interpret("(/ 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_two_arguments_returns_their_quotient() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(/ 84 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_many_arguments_returns_their_quotient() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Numeric(42);
    let actual = interpreter.interpret("(/ 1260 2 3 5)")?;
    assert_eq!(expected, actual);
    Ok(())
}

fn create_interpreter() -> Interpreter {
    let mut library_file = File::open("lib/base.bk").expect("Cannot open library file");
    let mut library_syntax = String::new();
    library_file
        .read_to_string(&mut library_syntax)
        .expect("Cannot read library file");
    Interpreter::with_library(&library_syntax).expect("Cannot create an interpreter with library")
}
