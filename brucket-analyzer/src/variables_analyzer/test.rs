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
use brucket_ast::ast::constant_value::{Boolean, ConstantVariant, Number};
use brucket_ast::ast::lambda::{Arity, Parameter};

type TestResult = Result<(), TestError>;
type TestError = Cow<'static, str>;

#[test]
fn test_let_variables_are_analyzed_correctly() -> TestResult {
    let node = Node::Let(Let::new(
        NodeId(0),
        "foo".to_owned(),
        Type::Any,
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(1),
            ConstantVariant::Numeric(Number::Integer("1".to_owned())),
        ))),
        Box::new(Node::Let(Let::new(
            NodeId(2),
            "bar".to_owned(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(3),
                ConstantVariant::Numeric(Number::FloatingPoint("1.1".to_owned())),
            ))),
            Box::new(Node::Identifier(Identifier::new(
                NodeId(4),
                Path::Simple("foo".to_owned()),
            ))),
        ))),
    ));
    let expected_node_variables = NodeVariables::new(
        maplit::hashmap! {
            "foo".to_owned() => Variable::new("foo".to_owned(), Type::Any),
            "bar".to_owned() => Variable::new("bar".to_owned(), Type::Any)
        },
        maplit::hashset! {
            "foo".to_owned()
        },
        HashSet::default(),
    );
    let expected_variables = Variables(maplit::hashmap! {
        NodeId(0) => NodeVariables::new(
            maplit::hashmap! {
                "foo".to_owned() => Variable::new("foo".to_owned(), Type::Any),
                "bar".to_owned() => Variable::new("bar".to_owned(), Type::Any)
            },
            maplit::hashset! {
                "foo".to_owned()
            },
            HashSet::default()
        ),
        NodeId(1) => NodeVariables::default(),
        NodeId(2) => NodeVariables::new(
            maplit::hashmap! {
                "bar".to_owned() => Variable::new("bar".to_owned(), Type::Any)
            },
            maplit::hashset! {
                "foo".to_owned()
            },
            maplit::hashset! {
                "foo".to_owned()
            }
        ),
        NodeId(3) => NodeVariables::default(),
        NodeId(4) => NodeVariables::new(
            HashMap::default(),
            maplit::hashset! {
                "foo".to_owned()
            },
            maplit::hashset! {
                "foo".to_owned()
            }
        ),
    });
    let mut variables_analyzer = VariablesAnalyzer::default();
    let (node_variables, variables) = variables_analyzer.analyze_variables(&node)?;
    assert_eq!(expected_node_variables, node_variables);
    assert_eq!(expected_variables, variables);
    Ok(())
}

#[test]
fn lambda_variables_are_analyzed_correctly() -> TestResult {
    let node = Node::Lambda(Lambda::new(
        NodeId(0),
        vec![Parameter::new(
            "foo".to_owned(),
            Type::Integer,
            Arity::Unary,
        )],
        Type::Integer,
        Box::new(Node::If(If::new(
            NodeId(1),
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(2),
                ConstantVariant::Boolean(Boolean::True),
            ))),
            Box::new(Node::Identifier(Identifier::new(
                NodeId(3),
                Path::Simple("foo".to_owned()),
            ))),
            Box::new(Node::Identifier(Identifier::new(
                NodeId(4),
                Path::Simple("bar".to_owned()),
            ))),
        ))),
    ));
    let expected_node_variables = NodeVariables::new(
        HashMap::default(),
        maplit::hashset! {
            "foo".to_owned(),
            "bar".to_owned()
        },
        maplit::hashset! {
            "bar".to_owned()
        },
    );
    let expected_variables = Variables(maplit::hashmap! {
        NodeId(0) => NodeVariables::new(
            HashMap::default(),
            maplit::hashset! {
                "foo".to_owned(),
                "bar".to_owned()
            },
            maplit::hashset! {
                "bar".to_owned()
            },
        ),
        NodeId(1) => NodeVariables::new(
            HashMap::default(),
            maplit::hashset! {
                "foo".to_owned(),
                "bar".to_owned()
            },
            maplit::hashset! {
                "foo".to_owned(),
                "bar".to_owned()
            },
        ),
        NodeId(2) => NodeVariables::default(),
        NodeId(3) => NodeVariables::new(
            HashMap::default(),
            maplit::hashset! {
                "foo".to_owned()
            },
            maplit::hashset! {
                "foo".to_owned()
            }
        ),
        NodeId(4) => NodeVariables::new(
            HashMap::default(),
            maplit::hashset! {
                "bar".to_owned()
            },
            maplit::hashset! {
                "bar".to_owned()
            }
        ),
    });
    let mut variables_analyzer = VariablesAnalyzer::default();
    let (node_variables, variables) = variables_analyzer.analyze_variables(&node)?;
    assert_eq!(expected_node_variables, node_variables);
    assert_eq!(expected_variables, variables);
    Ok(())
}
