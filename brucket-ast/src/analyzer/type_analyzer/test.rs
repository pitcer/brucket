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

#[test]
fn test_let_types_are_evaluated_correctly() -> Result<(), Cow<'static, str>> {
    let expected = TypedExpression::new(
        TypedExpressionType::Let(Let::new(
            "foo".into(),
            Type::Any,
            TypedExpression::new(
                TypedExpressionType::ConstantValue(ConstantValue::Numeric(Number::Integer(
                    "1".into(),
                ))),
                Type::Integer,
            )
            .into(),
            TypedExpression::new(
                TypedExpressionType::Let(Let::new(
                    "bar".into(),
                    Type::Any,
                    TypedExpression::new(
                        TypedExpressionType::ConstantValue(ConstantValue::Numeric(
                            Number::FloatingPoint("1.1".into()),
                        )),
                        Type::Float,
                    )
                    .into(),
                    TypedExpression::new(
                        TypedExpressionType::Identifier(Path::Simple("foo".into())),
                        Type::Integer,
                    )
                    .into(),
                )),
                Type::Integer,
            )
            .into(),
        )),
        Type::Integer,
    );
    let expression = Expression::Let(Let::new(
        "foo".into(),
        Type::Any,
        Expression::ConstantValue(ConstantValue::Numeric(Number::Integer("1".into()))).into(),
        Expression::Let(Let::new(
            "bar".into(),
            Type::Any,
            Expression::ConstantValue(ConstantValue::Numeric(Number::FloatingPoint("1.1".into())))
                .into(),
            Expression::Identifier(Path::Simple("foo".into())).into(),
        ))
        .into(),
    ));
    let mut environment = Environment::default();
    let actual = expression.into_typed(&mut environment)?;
    assert_eq!(expected, actual);
    Ok(())
}
