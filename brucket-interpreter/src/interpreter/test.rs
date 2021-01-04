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

use brucket_ast::ast::{
    ApplicationStrategy, Arity, ConstantValue, Expression, Number, Parameter, Path, Type,
};

use crate::value::Value;
use crate::value::{Closure, Numeric};

use super::*;

type TestResult = Result<(), String>;

#[test]
fn test_interpret_number() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("42")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_unit_function() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Unit;
    let actual = interpreter.interpret_with_base_library("()")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_simple_arithmetic_expression() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(3));
    let actual = interpreter.interpret_with_base_library("(+ 1 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_arithmetic_expression() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(9));
    let actual = interpreter.interpret_with_base_library("(+ (* 2 3) (- 7 4))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_expression_with_comment() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("# foobar\n(+ 40 2)\n#another comment")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_arithmetic_expression_with_variables() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library(
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
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library(
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
    let interpreter = Interpreter::default();
    let _result = interpreter.interpret_with_base_library("(/ 1 0)");
}

#[test]
fn test_if_expression_is_evaluated_lazily() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(if false (/ 1 0) 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_environment_is_cleared() {
    let interpreter = Interpreter::default();
    let expected = Err("Undefined variable: x".to_string());
    let actual = interpreter.interpret_with_base_library("(+ (let x 42 x) x)");
    assert_eq!(expected, actual);
}

#[test]
fn test_interpret_module() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Module(
        false,
        "foo".to_string(),
        environment! {
            "bar" => Value::Numeric(Numeric::Integer(1)),
            "barfoo" => Value::FunctionClosure(
                ApplicationStrategy::Eager,
                Closure::new(
                    vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
                    Box::new(Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("2".to_string())))),
                    Environment::new(),
            )),
            "foobar" => Value::Numeric(Numeric::Integer(3)),
            "fooo" => Value::FunctionClosure(
                ApplicationStrategy::Eager,
                Closure::new(
                    vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
                    Box::new(Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("4".to_string())))),
                    Environment::new(),
            ))
        },
    );
    let actual = interpreter.interpret_with_base_library(
        r#"
        (module foo
          (public constant bar 1)
          (public function barfoo [x] 2)
          (private function barr [x] 0)
          (private constant baar 0)
          (public constant foobar 3)
          (public function fooo [x] 4)
        )
        "#,
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_function() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::FunctionClosure(
        ApplicationStrategy::Eager,
        Closure::new(
            vec![Parameter::new("x".to_string(), Type::Any, Arity::Unary)],
            Box::from(Expression::Identifier(Path::Simple("x".to_string()))),
            Environment::new(),
        ),
    );
    let actual = interpreter.interpret_with_base_library("(function foo [x] x))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_comparison_functions() -> TestResult {
    let interpreter = Interpreter::default();
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret_with_base_library("(= 42 24)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(= 42 42)")?
    );
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret_with_base_library("(> 24 42)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(> 42 24)")?
    );
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret_with_base_library("(>= 24 42)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(>= 42 24)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(>= 42 42)")?
    );
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret_with_base_library("(< 42 24)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(< 24 42)")?
    );
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret_with_base_library("(<= 42 24)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(<= 24 42)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(<= 42 42)")?
    );
    Ok(())
}

#[test]
fn test_and_is_evaluated_lazily() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Boolean(false);
    let actual = interpreter.interpret_with_base_library("(and true false (/ 1 0))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_or_is_evaluated_lazily() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Boolean(true);
    let actual = interpreter.interpret_with_base_library("(or false true (/ 1 0))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_negate_function_negates_correctly() -> TestResult {
    let interpreter = Interpreter::default();
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret_with_base_library("(negate true)")?
    );
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(negate false)")?
    );
    Ok(())
}

#[test]
fn test_recursive_lambda() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(0));
    let actual = interpreter.interpret_with_base_library(
        r#"
        (let foo
          (=> [x]
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
    let interpreter = Interpreter::default();
    assert_eq!(
        Value::Numeric(Numeric::Integer(42)),
        interpreter.interpret_with_base_library("(pair::first (pair::new 42 24))")?
    );
    assert_eq!(
        Value::Numeric(Numeric::Integer(24)),
        interpreter.interpret_with_base_library("(pair::second (pair::new 42 24))")?
    );
    Ok(())
}

#[test]
fn test_is_null() -> TestResult {
    let interpreter = Interpreter::default();
    assert_eq!(
        Value::Boolean(true),
        interpreter.interpret_with_base_library("(is_null null)")?
    );
    assert_eq!(
        Value::Boolean(false),
        interpreter.interpret_with_base_library("(is_null 42)")?
    );
    Ok(())
}

#[test]
fn test_variadic_parameter() -> TestResult {
    let interpreter = Interpreter::default();
    assert_eq!(
        Value::Pair(
            Box::new(Value::Numeric(Numeric::Integer(2))),
            Box::new(Value::Pair(
                Box::new(Value::Numeric(Numeric::Integer(3))),
                Box::new(Value::Pair(
                    Box::new(Value::Numeric(Numeric::Integer(4))),
                    Box::new(Value::Null),
                )),
            )),
        ),
        interpreter.interpret_with_base_library("((=> [x xs... y] xs) 1 2 3 4)")?
    );
    assert_eq!(
        Value::Numeric(Numeric::Integer(1)),
        interpreter.interpret_with_base_library("((=> [x xs... y] x) 1 2 3 4)")?
    );
    Ok(())
}

#[test]
fn test_call_other_members_in_module() -> TestResult {
    let library = r#"
        (module test
          (constant X 42)

          (function foo [x]
            x)

          (public function foobar []
            (foo (bar X)))

          (function bar [x]
            x))
        "#;
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual =
        interpreter.interpret_with_base_library_and_modules("(test::foobar)", vec![library])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_call_other_members_in_module_including_recursive_function() -> TestResult {
    let library = r#"
        (module test
          (constant X 42)

          (function foo [x]
            (iter x STEP))

          (public function bar []
            (foo X))

          (function iter [x y]
            (if (internal is_greater y 0)
              (iter x (internal subtract y 1))
              x))

          (constant STEP 3))
        "#;
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual =
        interpreter.interpret_with_base_library_and_modules("(test::bar)", vec![library])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_add_with_zero_arguments_returns_zero() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(0));
    let actual = interpreter.interpret_with_base_library("(+)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_add_with_one_argument_returns_that_argument() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(+ 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_add_with_two_arguments_returns_their_sum() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(+ 40 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_add_with_many_arguments_returns_their_sum() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(+ 18 2 9 13)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_zero_arguments_returns_zero() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(0));
    let actual = interpreter.interpret_with_base_library("(-)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_one_argument_returns_that_argument_with_opposite_sign() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(-42));
    let actual = interpreter.interpret_with_base_library("(- 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_two_arguments_returns_their_difference() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(40));
    let actual = interpreter.interpret_with_base_library("(- 42 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_many_arguments_returns_their_difference() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(10));
    let actual = interpreter.interpret_with_base_library("(- 42 10 20 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_zero_arguments_returns_one() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(1));
    let actual = interpreter.interpret_with_base_library("(*)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_one_argument_returns_that_argument() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(* 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_two_arguments_returns_their_product() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(* 21 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_many_arguments_returns_their_product() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(* 7 3 2 1)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_zero_arguments_returns_one() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(1));
    let actual = interpreter.interpret_with_base_library("(/)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_one_argument_returns_inverse_of_that_argument() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(1 / 42));
    let actual = interpreter.interpret_with_base_library("(/ 42)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_two_arguments_returns_their_quotient() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(/ 84 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_many_arguments_returns_their_quotient() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library("(/ 1260 2 3 5)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_lazy_function_application() -> TestResult {
    let library = r#"
        (module test
          (public lazy function foo [x]
            (x)))
        "#;
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual =
        interpreter.interpret_with_base_library_and_modules("(test::foo 42)", vec![library])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_lazy_function_arguments_are_evaluated_lazily() -> TestResult {
    let library = r#"
        (module test
          (public lazy function foo [x a y b z]
            (internal add (a) (b))))
        "#;
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let syntax = "(test::foo (/ 1 0) 20 (/ 1 0) 22 (/ 1 0))";
    let actual = interpreter.interpret_with_base_library_and_modules(syntax, vec![library])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_and_is_false_if_false_argument_exists() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Boolean(false);
    let actual = interpreter.interpret_with_base_library("(and true true false true)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_and_is_true_if_every_argument_is_true() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Boolean(true);
    let actual = interpreter.interpret_with_base_library("(and true true true true)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_or_is_true_if_true_argument_exists() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Boolean(true);
    let actual = interpreter.interpret_with_base_library("(or false false true false)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_or_is_false_if_every_argument_is_false() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Boolean(false);
    let actual = interpreter.interpret_with_base_library("(or false false false false)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_static_module() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Module(
        true,
        "foo".to_string(),
        environment! {
            "bar" => Value::Numeric(Numeric::Integer(1))
        },
    );
    let actual = interpreter.interpret_with_base_library(
        r#"
        (static module foo
          (public constant bar 1)
        )
        "#,
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_static_module_members_do_not_require_path_to_access() -> TestResult {
    let module = r#"
        (static module test
          (constant X 42)

          (function foo [x]
            x)

          (public function foobar []
            (foo (bar X)))

          (function bar [x]
            x))
        "#;
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = interpreter.interpret_with_base_library_and_modules("(foobar)", vec![module])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_list_new_function_evaluates_to_pair_list() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Pair(
        Box::from(Value::Numeric(Numeric::Integer(1))),
        Box::new(Value::Pair(
            Box::from(Value::Numeric(Numeric::Integer(2))),
            Box::from(Value::Pair(
                Box::from(Value::Numeric(Numeric::Integer(3))),
                Box::from(Value::Null),
            )),
        )),
    );
    let actual = interpreter.interpret_with_base_library("(list::new 1 2 3)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_list_get_length_function_returns_list_length() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Numeric(Numeric::Integer(3));
    let actual = interpreter.interpret_with_base_library("(list::get_length (list::new 1 2 3))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_sort_returns_sorted_list() -> TestResult {
    let interpreter = Interpreter::default();
    let expected = Value::Pair(
        Box::from(Value::Numeric(Numeric::Integer(1))),
        Box::new(Value::Pair(
            Box::from(Value::Numeric(Numeric::Integer(2))),
            Box::from(Value::Pair(
                Box::from(Value::Numeric(Numeric::Integer(3))),
                Box::from(Value::Null),
            )),
        )),
    );
    let actual = interpreter.interpret_with_base_library("(sort::sort 3 1 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}
