use brucket_ast::quote;

use super::*;

type TestResult = Result<(), TestError>;
type TestError = Cow<'static, str>;

#[test]
fn test_let_types_are_evaluated_correctly() -> TestResult {
    let node = quote! {
        (0: let foo: any (1: 1)
            (2: let bar: any (3: 1.1)
                (4: foo)))
    };
    let expected = NodeTypes(maplit::hashmap! {
        NodeId(0) => Type::Integer,
        NodeId(1) => Type::Integer,
        NodeId(2) => Type::Integer,
        NodeId(3) => Type::Float,
        NodeId(4) => Type::Integer,
    });
    let mut type_analyzer = TypeAnalyzer::default();
    let (node_type, node_types) = type_analyzer.analyze_types(&node)?;
    assert_eq!(Type::Integer, node_type);
    assert_eq!(expected, node_types);
    Ok(())
}
