use brucket_ast::quote;

use super::*;

type TestResult = Result<(), TestError>;
type TestError = Cow<'static, str>;

#[test]
fn test_let_variables_are_analyzed_correctly() -> TestResult {
    let node = quote! {
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
    let node = quote! {
        (0: lambda [(foo: int)] -> int (1: if (2: true) (3: foo) (4: bar)))
    };
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
