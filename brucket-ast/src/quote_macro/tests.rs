use crate::ast_type::{LambdaType, Type};
use crate::constant_value::{Boolean, ConstantValue, ConstantVariant, Number};
use crate::function::{ApplicationStrategy, Function, InternalFunction};
use crate::lambda::{Arity, Lambda, Parameter};
use crate::path::Path;
use crate::quote;
use crate::{Application, Constant, Identifier, If, Let, Module, Node, NodeId, Visibility};

#[test]
fn quoted_integer_is_integer_constant_value() {
    let expected = Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Numeric(Number::Integer("42".to_owned())),
    ));
    let actual = quote!(42);
    assert_eq!(expected, actual);
}

#[test]
fn quoted_float_is_float_constant_value() {
    let expected = Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Numeric(Number::FloatingPoint("42.24".to_owned())),
    ));
    let actual = quote!(42.24);
    assert_eq!(expected, actual);
}

#[test]
fn quoted_true_is_boolean_constant_value() {
    let expected = Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Boolean(Boolean::True),
    ));
    let actual = quote!(true);
    assert_eq!(expected, actual);
}

#[test]
fn quoted_false_is_boolean_constant_value() {
    let expected = Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::Boolean(Boolean::False),
    ));
    let actual = quote!(false);
    assert_eq!(expected, actual);
}

#[test]
fn quoted_string_is_string_constant_value() {
    let expected = Node::ConstantValue(ConstantValue::new(
        NodeId(0),
        ConstantVariant::String("foobar".to_owned()),
    ));
    let actual = quote!("foobar");
    assert_eq!(expected, actual);
}

#[test]
fn quoted_unit_is_unit_constant_value() {
    let expected = Node::ConstantValue(ConstantValue::new(NodeId(0), ConstantVariant::Unit));
    let actual = quote!(());
    assert_eq!(expected, actual);
}

#[test]
fn quoted_null_is_null_constant_value() {
    let expected = Node::ConstantValue(ConstantValue::new(NodeId(0), ConstantVariant::Null));
    let actual = quote!(null);
    assert_eq!(expected, actual);
}

#[test]
fn quoted_simple_identifier_is_identifier_node() {
    let expected = Node::Identifier(Identifier::new(
        NodeId(0),
        Path::Simple("foobar".to_owned()),
    ));
    let actual = quote!(foobar);
    assert_eq!(expected, actual);
}

#[test]
fn quoted_complex_identifier_is_identifier_node() {
    let expected = Node::Identifier(Identifier::new(
        NodeId(0),
        Path::Complex(vec![
            "foo".to_owned(),
            "bar".to_owned(),
            "foobar".to_owned(),
        ]),
    ));
    let actual = quote!(foo::bar::foobar);
    assert_eq!(expected, actual);
}

#[test]
fn quoted_application_is_application_node() {
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("foobar".to_owned()),
        ))),
        Vec::new(),
    ));
    let actual = quote!((foobar));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_unary_application_is_application_node() {
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("foobar".to_owned()),
        ))),
        vec![Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_owned())),
        ))],
    ));
    let actual = quote!((foobar 42));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_multi_parameter_application_is_application_node() {
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("foobar".to_owned()),
        ))),
        vec![
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_owned())),
            )),
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("24".to_owned())),
            )),
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("0".to_owned())),
            )),
        ],
    ));
    let actual = quote!((foobar 42 24 0));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_application_simple_complex_path_identifier_is_application_node() {
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Complex(vec!["foo".to_owned(), "foobar".to_owned()]),
        ))),
        vec![Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::String("foo".to_owned()),
        ))],
    ));
    let actual = quote!(((foo::foobar) "foo"));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_application_complex_path_identifier_is_application_node() {
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Complex(vec![
                "foo".to_owned(),
                "bar".to_owned(),
                "foobar".to_owned(),
                "barfoo".to_owned(),
            ]),
        ))),
        Vec::new(),
    ));
    let actual = quote! {
        ((foo::bar::foobar::barfoo))
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_let_is_let_node() {
    let expected = Node::Let(Let::new(
        NodeId(0),
        "x".to_owned(),
        Type::Any,
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_owned())),
        ))),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_owned()),
        ))),
    ));
    let actual = quote!((let x 42 x));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_typed_let_is_let_node() {
    let expected = Node::Let(Let::new(
        NodeId(0),
        "x".to_owned(),
        Type::Integer,
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_owned())),
        ))),
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_owned()),
        ))),
    ));
    let actual = quote!((let x: int 42 x));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_if_is_if_node() {
    let expected = Node::If(If::new(
        NodeId(0),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Boolean(Boolean::True),
        ))),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_owned())),
        ))),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("24".to_owned())),
        ))),
    ));
    let actual = quote!((if true 42 24));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_empty_parameters_lambda_is_lambda_node() {
    let expected = Node::Lambda(Lambda::new(
        NodeId(0),
        Vec::new(),
        Type::Any,
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_owned()),
        ))),
    ));
    let actual = quote!((lambda [] x));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_single_parameter_lambda_is_lambda_node() {
    let expected = Node::Lambda(Lambda::new(
        NodeId(0),
        vec![Parameter::new("x".to_owned(), Type::Any, Arity::Unary)],
        Type::Any,
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_owned()),
        ))),
    ));
    let actual = quote!((lambda [x] x));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_multi_parameters_lambda_is_lambda_node() {
    let expected = Node::Lambda(Lambda::new(
        NodeId(0),
        vec![
            Parameter::new("x".to_owned(), Type::Any, Arity::Unary),
            Parameter::new("y".to_owned(), Type::Any, Arity::Unary),
            Parameter::new("z".to_owned(), Type::Any, Arity::Unary),
        ],
        Type::Any,
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_owned()),
        ))),
    ));
    let actual = quote!((lambda [x y z] x));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_lambda_with_variadic_parameter_is_lambda_node() {
    let expected = Node::Lambda(Lambda::new(
        NodeId(0),
        vec![Parameter::new("xs".to_owned(), Type::Any, Arity::Variadic)],
        Type::Any,
        Box::new(Node::Identifier(Identifier::new(
            NodeId(0),
            Path::Simple("x".to_owned()),
        ))),
    ));
    let actual = quote!((lambda [(xs: any...)] x));
    assert_eq!(expected, actual);
}

#[test]
fn quoted_lambda_application_is_application_node() {
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Lambda(Lambda::new(
            NodeId(1),
            vec![
                Parameter::new("x".to_owned(), Type::Any, Arity::Unary),
                Parameter::new("y".to_owned(), Type::Any, Arity::Unary),
                Parameter::new("z".to_owned(), Type::Any, Arity::Variadic),
            ],
            Type::Any,
            Box::new(Node::Identifier(Identifier::new(
                NodeId(0),
                Path::Simple("z".to_owned()),
            ))),
        ))),
        vec![
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("1".to_owned())),
            )),
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("2".to_owned())),
            )),
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("3".to_owned())),
            )),
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("4".to_owned())),
            )),
            Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("5".to_owned())),
            )),
        ],
    ));
    let actual = quote! {
        ((1: lambda [(x: any) (y: any) (z: any...)] -> any z) 1 2 3 4 5)
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_function_is_function_node() {
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Private,
        ApplicationStrategy::Eager,
        "foo".to_owned(),
        Lambda::new(
            NodeId(0),
            vec![
                Parameter::new("x".to_owned(), Type::Any, Arity::Unary),
                Parameter::new("y".to_owned(), Type::Any, Arity::Unary),
                Parameter::new("z".to_owned(), Type::Any, Arity::Unary),
            ],
            Type::Any,
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("bar".to_owned()),
                ))),
                vec![Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("42".to_owned())),
                ))],
            ))),
        ),
    ));
    let actual = quote! {
        @node Function
        (function foo [x y z] (bar 42))
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_function_with_types_is_function_node() {
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Private,
        ApplicationStrategy::Eager,
        "foo".to_owned(),
        Lambda::new(
            NodeId(0),
            vec![
                Parameter::new("x".to_owned(), Type::Any, Arity::Unary),
                Parameter::new("y".to_owned(), Type::Symbol("Bar".to_owned()), Arity::Unary),
                Parameter::new("z".to_owned(), Type::Integer, Arity::Variadic),
            ],
            Type::Boolean,
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("bar".to_owned()),
                ))),
                vec![Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("42".to_owned())),
                ))],
            ))),
        ),
    ));
    let actual = quote! {
        @node Function
        (0: private eager function foo [(x: any) (y: Bar) (z: int...)] -> bool
            0: (bar 42))
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_public_function_is_function_node() {
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Public,
        ApplicationStrategy::Eager,
        "foo".to_owned(),
        Lambda::new(
            NodeId(0),
            Vec::new(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_owned())),
            ))),
        ),
    ));
    let actual = quote! {
        @node Function
        (0: public eager function foo [] -> any 0: 42)
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_function_with_lambda_type_is_function_node() {
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Private,
        ApplicationStrategy::Eager,
        "foobar".to_owned(),
        Lambda::new(
            NodeId(0),
            vec![
                Parameter::new("x".to_owned(), Type::Any, Arity::Unary),
                Parameter::new(
                    "y".to_owned(),
                    Type::Lambda(LambdaType::new(
                        vec![Type::Integer, Type::Boolean],
                        Box::new(Type::Symbol("Test".to_owned())),
                    )),
                    Arity::Unary,
                ),
                Parameter::new("z".to_owned(), Type::Integer, Arity::Variadic),
            ],
            Type::Lambda(LambdaType::new(vec![], Box::new(Type::Integer))),
            Box::new(Node::Application(Application::new(
                NodeId(0),
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("bar".to_owned()),
                ))),
                vec![Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("42".to_owned())),
                ))],
            ))),
        ),
    ));
    let actual = quote! {
        @node Function
        (0: private eager function foobar
            [(x: any) (y: (int bool -> Test)) (z: int...)] -> (-> int)
            0: (bar 42))
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_lazy_function_is_function_node() {
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Private,
        ApplicationStrategy::Lazy,
        "foo".to_owned(),
        Lambda::new(
            NodeId(0),
            Vec::new(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_owned())),
            ))),
        ),
    ));
    let actual = quote! {
        @node Function
        (0: private lazy function foo [] -> any 0: 42)
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_public_lazy_function_is_function_node() {
    let expected = Node::Function(Function::new(
        NodeId(0),
        Visibility::Public,
        ApplicationStrategy::Lazy,
        "foo".to_owned(),
        Lambda::new(
            NodeId(0),
            Vec::new(),
            Type::Any,
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_owned())),
            ))),
        ),
    ));
    let actual = quote! {
        @node Function
        (0: public lazy function foo [] -> any 0: 42)
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_function_application_is_application_node() {
    let expected = Node::Application(Application::new(
        NodeId(0),
        Box::new(Node::Function(Function::new(
            NodeId(0),
            Visibility::Private,
            ApplicationStrategy::Lazy,
            "foo".to_owned(),
            Lambda::new(
                NodeId(1),
                vec![Parameter::new("x".to_owned(), Type::Any, Arity::Unary)],
                Type::Any,
                Box::new(Node::Identifier(Identifier::new(
                    NodeId(0),
                    Path::Simple("x".to_owned()),
                ))),
            ),
        ))),
        vec![Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_owned())),
        ))],
    ));
    let actual = quote! {
        ((@node Function (0: private lazy function foo [x] -> any 1: x)) 42)
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_internal_function_is_internal_function_node() {
    let expected = Node::InternalFunction(InternalFunction::new(
        NodeId(0),
        Visibility::Public,
        ApplicationStrategy::Lazy,
        "foobar".to_owned(),
        vec![
            Parameter::new("x".to_owned(), Type::Any, Arity::Unary),
            Parameter::new("y".to_owned(), Type::Symbol("Bar".to_owned()), Arity::Unary),
            Parameter::new("z".to_owned(), Type::Integer, Arity::Variadic),
        ],
        Type::Boolean,
    ));
    let actual = quote! {
        @node InternalFunction
        (0: public lazy internal_function foobar [(x: any) (y: Bar) (z: int...)] -> bool)
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_constant_is_constant_node() {
    let expected = Node::Constant(Constant::new(
        NodeId(0),
        Visibility::Private,
        "foo".to_owned(),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_owned())),
        ))),
    ));
    let actual = quote! {
        @node Constant
        (constant foo 42)
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_public_constant_is_constant_node() {
    let expected = Node::Constant(Constant::new(
        NodeId(0),
        Visibility::Public,
        "foo".to_owned(),
        Box::new(Node::ConstantValue(ConstantValue::new(
            NodeId(0),
            ConstantVariant::Numeric(Number::Integer("42".to_owned())),
        ))),
    ));
    let actual = quote! {
        @node Constant
        (0: public constant foo 42)
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_module_is_module_node() {
    let expected = Node::Module(Module::new(
        NodeId(0),
        false,
        "foo".to_owned(),
        vec![Function::new(
            NodeId(0),
            Visibility::Private,
            ApplicationStrategy::Eager,
            "x".to_owned(),
            Lambda::new(
                NodeId(0),
                Vec::new(),
                Type::Any,
                Box::new(Node::ConstantValue(ConstantValue::new(
                    NodeId(0),
                    ConstantVariant::Numeric(Number::Integer("42".to_owned())),
                ))),
            ),
        )],
        vec![InternalFunction::new(
            NodeId(0),
            Visibility::Private,
            ApplicationStrategy::Eager,
            "z".to_owned(),
            Vec::new(),
            Type::Any,
        )],
        vec![Constant::new(
            NodeId(0),
            Visibility::Private,
            "y".to_owned(),
            Box::new(Node::ConstantValue(ConstantValue::new(
                NodeId(0),
                ConstantVariant::Numeric(Number::Integer("42".to_owned())),
            ))),
        )],
    ));
    let actual = quote! {
        @node Module
        (module foo
            (
                (function x [] 42)
            )
            (
                (internal_function z [])
            )
            (
                (constant y 42)
            )
        )
    };
    assert_eq!(expected, actual);
}

#[test]
fn quoted_static_module_is_module_node() {
    let expected = Node::Module(Module::new(
        NodeId(0),
        true,
        "foo".to_owned(),
        Vec::new(),
        Vec::new(),
        Vec::new(),
    ));
    let actual = quote! {
        @node Module
        (0: static module foo () () ())
    };
    assert_eq!(expected, actual);
}

#[test]
fn lambda_type_is_quoted_correctly() {
    assert_eq!(
        Type::Lambda(LambdaType::new(Vec::default(), Box::new(Type::Integer))),
        quote!(@type (-> int))
    );
    assert_eq!(
        Type::Lambda(LambdaType::new(
            vec![Type::Integer],
            Box::new(Type::Integer)
        )),
        quote!(@type (int -> int))
    );
    assert_eq!(
        Type::Lambda(LambdaType::new(
            vec![Type::Integer, Type::Integer],
            Box::new(Type::Integer)
        )),
        quote!(@type (int int -> int))
    );
    assert_eq!(
        Type::Lambda(LambdaType::new(
            vec![
                Type::Integer,
                Type::Lambda(LambdaType::new(
                    vec![Type::Integer],
                    Box::new(Type::Integer)
                ))
            ],
            Box::new(Type::Integer)
        )),
        quote!(@type ((int (int -> int)) -> int))
    );
}
