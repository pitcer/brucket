use brucket_ast::constant_value::{Boolean, ConstantVariant};
use brucket_ast::lambda::{Arity, Parameter};
use brucket_quote::brucket;

use super::*;

type TestResult = Result<(), TestError>;
type TestError = Cow<'static, str>;

#[test]
fn test_let_variables_are_analyzed_correctly() -> TestResult {
    let node = brucket! {
        (0: let foo: any (1: 1)
            (2: let bar: any (3: 1.1)
                (4: foo)))
    };
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
