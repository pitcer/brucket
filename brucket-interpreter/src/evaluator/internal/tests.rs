use super::*;

type TestResult = Result<(), String>;

#[test]
fn test_add_with_two_arguments_returns_their_sum() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = add(maplit::hashmap![
        "first".to_owned() => Value::Numeric(Numeric::Integer(40)),
        "second".to_owned() => Value::Numeric(Numeric::Integer(2)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_subtract_with_two_arguments_returns_their_difference() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(40));
    let actual = subtract(maplit::hashmap![
        "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
        "second".to_owned() => Value::Numeric(Numeric::Integer(2)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_multiply_with_two_arguments_returns_their_product() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = multiply(maplit::hashmap![
        "first".to_owned() => Value::Numeric(Numeric::Integer(21)),
        "second".to_owned() => Value::Numeric(Numeric::Integer(2)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_divide_with_two_arguments_returns_their_quotient() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(42));
    let actual = divide(maplit::hashmap![
        "first".to_owned() => Value::Numeric(Numeric::Integer(84)),
        "second".to_owned() => Value::Numeric(Numeric::Integer(2)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_remainder_with_two_arguments_returns_their_remainder() -> TestResult {
    let expected = Value::Numeric(Numeric::Integer(3));
    let actual = remainder(maplit::hashmap![
        "first".to_owned() => Value::Numeric(Numeric::Integer(45)),
        "second".to_owned() => Value::Numeric(Numeric::Integer(42)),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_is_equal() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_equal(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(24))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_equal(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(42))
        ])?
    );
    Ok(())
}

#[test]
fn test_is_greater() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_greater(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(24)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(42))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_greater(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(24))
        ])?
    );
    Ok(())
}

#[test]
fn test_is_greater_or_equal() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_greater_or_equal(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(24)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(42))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_greater_or_equal(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(24))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_greater_or_equal(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(42))
        ])?
    );
    Ok(())
}

#[test]
fn test_is_less() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_less(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(24))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_less(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(24)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(42))
        ])?
    );
    Ok(())
}

#[test]
fn test_is_less_or_equal() -> TestResult {
    assert_eq!(
        Value::Boolean(false),
        is_less_or_equal(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(24))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_less_or_equal(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(24)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(42))
        ])?
    );
    assert_eq!(
        Value::Boolean(true),
        is_less_or_equal(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(42))
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
        pair::new(maplit::hashmap![
            "first".to_owned() => Value::Numeric(Numeric::Integer(42)),
            "second".to_owned() => Value::Numeric(Numeric::Integer(24))
        ])?
    );
    Ok(())
}

#[test]
fn test_pair_first() -> TestResult {
    assert_eq!(
        Value::Numeric(Numeric::Integer(42)),
        pair::first(maplit::hashmap![
        "pair".to_owned() => Value::Pair(
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
        pair::second(maplit::hashmap![
        "pair".to_owned() => Value::Pair(
            Box::new(Value::Numeric(Numeric::Integer(42))),
            Box::new(Value::Numeric(Numeric::Integer(24)))
        )])?
    );
    Ok(())
}
