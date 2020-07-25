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

use brucket::evaluator::Value;
use brucket::lexer::Lexer;
use brucket::{evaluator, parser};

#[test]
fn test_interpret_number() -> Result<(), String> {
    let expected = Value::Numeric(42);
    let actual = interpret("42")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_unit_function() -> Result<(), String> {
    let expected = Value::Unit;
    let actual = interpret("()")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_simple_arithmetic_expression() -> Result<(), String> {
    let expected = Value::Numeric(3);
    let actual = interpret("(+ 1 2)")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_arithmetic_expression() -> Result<(), String> {
    let expected = Value::Numeric(9);
    let actual = interpret("(+ (* 2 3) (- 7 4))")?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_interpret_expression_with_comment() -> Result<(), String> {
    let expected = Value::Numeric(42);
    let actual = interpret("# foobar\n(+ 40 2)\n#another comment")?;
    assert_eq!(expected, actual);
    Ok(())
}

fn interpret(syntax: &str) -> Result<Value, String> {
    let lexer = Lexer::default();
    let tokenized = lexer.tokenize(syntax);
    let parsed = parser::parse(&tokenized)?;
    evaluator::evaluate(&parsed)
}
