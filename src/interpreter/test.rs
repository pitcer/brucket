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
use crate::evaluator::{Closure, Environment};
use crate::parser::{Constant, Expression};

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
        maplit::hashmap! {
        "bar".to_string() => Value::Numeric(1),
        "barfoo".to_string() => Value::Closure(Closure::Parametrized(
            "x".to_string(),
            Expression::Constant(Constant::Numeric(2)),
            Environment::new(),
        )),
        "foobar".to_string() => Value::Numeric(3),
        "fooo".to_string() => Value::Closure(Closure::Parametrized(
            "x".to_string(),
            Expression::Constant(Constant::Numeric(4)),
            Environment::new(),
        ))
        },
    );
    let actual = interpreter.interpret(
        r#"
        (module foo
          (constant bar 1)
          (function barfoo |x| 2)
          (constant foobar 3)
          (function fooo |x| 4)
        )
        "#,
    )?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_function() -> TestResult {
    let interpreter = create_interpreter();
    let expected = Value::Identified(
        "foo".to_string(),
        Box::new(Value::Closure(Closure::Parametrized(
            "x".to_string(),
            Expression::Identifier("x".to_string()),
            Environment::new(),
        ))),
    );
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

fn create_interpreter() -> Interpreter {
    let library_syntax = r#"
    (module base
      (function add |first second|
        (internal add first second))

      (constant + add)

      (function subtract |first second|
        (internal subtract first second))

      (constant - subtract)

      (function multiply |first second|
        (internal multiply first second))

      (constant * multiply)

      (function divide |first second|
        (internal divide first second))

      (constant / divide)

      (function remainder |first second|
        (internal remainder first second))

      (constant % remainder)

      (function is_equal |first second|
        (internal is_equal first second))

      (constant = is_equal)

      (function is_greater |first second|
        (internal is_greater first second))

      (constant > is_greater)

      (function is_greater_or_equal |first second|
        (internal is_greater_or_equal first second))

      (constant >= is_greater_or_equal)

      (function is_less |first second|
        (internal is_less first second))

      (constant < is_less)

      (function is_less_or_equal |first second|
        (internal is_less_or_equal first second))

      (constant <= is_less_or_equal)
    )"#;
    Interpreter::with_library(library_syntax).expect("Cannot create interpreter with library")
}
