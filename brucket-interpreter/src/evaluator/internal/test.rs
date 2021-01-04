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
fn test_add_with_two_arguments_returns_their_sum() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = add(vec![
        Value::Numeric(Numeric::Integer(40)),
        Value::Numeric(Numeric::Integer(2)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_two_arguments_returns_their_difference() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(40));
    let actual = subtract(vec![
        Value::Numeric(Numeric::Integer(42)),
        Value::Numeric(Numeric::Integer(2)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_two_arguments_returns_their_product() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = multiply(vec![
        Value::Numeric(Numeric::Integer(21)),
        Value::Numeric(Numeric::Integer(2)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_two_arguments_returns_their_quotient() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = divide(vec![
        Value::Numeric(Numeric::Integer(84)),
        Value::Numeric(Numeric::Integer(2)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_remainder_with_two_arguments_returns_their_remainder() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(3));
    let actual = remainder(vec![
        Value::Numeric(Numeric::Integer(45)),
        Value::Numeric(Numeric::Integer(42)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_is_equal() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_equal(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(24))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_equal(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(42))
        ])?
    );
    Ok(())
}

#[test]
fn test_is_greater() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_greater(vec![
            Value::Numeric(Numeric::Integer(24)),
            Value::Numeric(Numeric::Integer(42))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_greater(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(24))
        ])?
    );
    Ok(())
}

#[test]
fn test_is_greater_or_equal() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_greater_or_equal(vec![
            Value::Numeric(Numeric::Integer(24)),
            Value::Numeric(Numeric::Integer(42))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_greater_or_equal(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(24))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_greater_or_equal(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(42))
        ])?
    );
    Ok(())
}

#[test]
fn test_is_less() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_less(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(24))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_less(vec![
            Value::Numeric(Numeric::Integer(24)),
            Value::Numeric(Numeric::Integer(42))
        ])?
    );
    Ok(())
}

#[test]
fn test_is_less_or_equal() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_less_or_equal(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(24))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_less_or_equal(vec![
            Value::Numeric(Numeric::Integer(24)),
            Value::Numeric(Numeric::Integer(42))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_less_or_equal(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(42))
        ])?
    );
    Ok(())
}

#[test]
fn test_pair_new() -> TestResult {
    assert_eq!(
        Value::Pair(
            Box::new(Value::Numeric(Numeric::Integer(42))),
            Box::new(Value::Numeric(Numeric::Integer(24)))
        ),
        pair::new(vec![
            Value::Numeric(Numeric::Integer(42)),
            Value::Numeric(Numeric::Integer(24))
        ])?
    );
    Ok(())
}

#[test]
fn test_pair_first() -> TestResult {
    assert_eq!(
        Value::Numeric(Numeric::Integer(42)),
        pair::first(vec![Value::Pair(
            Box::new(Value::Numeric(Numeric::Integer(42))),
            Box::new(Value::Numeric(Numeric::Integer(24)))
        )])?
    );
    Ok(())
}

#[test]
fn test_pair_second() -> TestResult {
    assert_eq!(
        Value::Numeric(Numeric::Integer(24)),
        pair::second(vec![Value::Pair(
            Box::new(Value::Numeric(Numeric::Integer(42))),
            Box::new(Value::Numeric(Numeric::Integer(24)))
        )])?
    );
    Ok(())
}
