use brucket_ast::quote;

use super::*;

type TestResult = Result<(), Cow<'static, str>>;

#[test]
fn test_parsed_number_token_is_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!(42);
    let actual = parser.parse(vec![Token::Number(NumberToken::Integer("42".to_owned()))])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_boolean_token_is_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!(true);
    let actual = parser.parse(vec![Token::Boolean(BooleanToken::True)])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_string_token_is_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!("foobar");
    let actual = parser.parse(vec![Token::String("foobar".to_owned())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_symbol_token_is_symbol_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!(foobar);
    let actual = parser.parse(vec![Token::Symbol("foobar".to_owned())])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_unit_tokens_are_unit_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!(());
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_null_token_is_null_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!(null);
    let actual = parser.parse(vec![Token::Null])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_constant_function_tokens_are_application_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((foobar));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_unary_function_tokens_are_application_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((foobar 42));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_multi_parameter_function_tokens_are_application_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((foobar 42 24 0));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foobar".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Number(NumberToken::Integer("24".to_owned())),
        Token::Number(NumberToken::Integer("0".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_let_tokens_are_let_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((let x 42 x));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Let),
        Token::Symbol("x".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Symbol("x".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_if_tokens_are_if_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((if true 42 24));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::If),
        Token::Boolean(BooleanToken::True),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Number(NumberToken::Integer("24".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_empty_parameters_lambda_tokens_are_lambda_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((lambda [] x));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Symbol("x".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_single_parameter_lambda_tokens_are_lambda_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((lambda [x] x));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_owned()),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Symbol("x".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_multi_parameters_lambda_tokens_are_lambda_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((lambda [x y z] x));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_owned()),
        Token::Symbol("y".to_owned()),
        Token::Symbol("z".to_owned()),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Symbol("x".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_module_tokens_are_module_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Module
        (module foo
            ((function x [] 42))
            ((internal_function z []))
            ((constant y 42))
        )
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Module),
        Token::Symbol("foo".to_owned()),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("x".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Internal),
        Token::Keyword(Keyword::Function),
        Token::Symbol("z".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Constant),
        Token::Symbol("y".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_function_tokens_are_function_expressions() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Function
        (function foo [x y z] (bar 42))
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_owned()),
        Token::Symbol("y".to_owned()),
        Token::Symbol("z".to_owned()),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("bar".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_constant_tokens_are_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Constant
        (constant foo 42)
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Constant),
        Token::Symbol("foo".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_lambda_with_variadic_parameter_tokens_are_lambda_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        (lambda [(xs: any...)] x)
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Lambda),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("xs".to_owned()),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Symbol("x".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_public_function_tokens_are_public_function_expressions() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Function
        (0: public eager function foo [] -> any 0: 42)
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Public),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_public_constant_tokens_are_constant_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Constant
        (0: public constant foo 42)
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Public),
        Token::Keyword(Keyword::Constant),
        Token::Symbol("foo".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_lazy_function_tokens_are_lazy_function() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Function
        (0: private lazy function foo [] -> any 0: 42)
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Lazy),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_public_lazy_function_tokens_are_public_lazy_function() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Function
        (0: public lazy function foo [] -> any 0: 42)
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Public),
        Token::Modifier(Modifier::Lazy),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_internal_function_tokens_are_internal_function() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node InternalFunction
        (0: public lazy internal_function foobar [(x: any) (y: Bar) (z: int...)] -> bool)
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Public),
        Token::Modifier(Modifier::Internal),
        Token::Modifier(Modifier::Lazy),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foobar".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_owned()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Any),
        Token::Symbol("y".to_owned()),
        Token::Operator(Operator::Type),
        Token::Symbol("Bar".to_owned()),
        Token::Symbol("z".to_owned()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::PrimitiveType(PrimitiveType::Boolean),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_application_simple_path_identifier_tokens_are_application_expression() -> TestResult
{
    let mut parser = Parser::default();
    let expected = quote! {
        ((foo::foobar) "foo")
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foo".to_owned()),
        Token::Operator(Operator::Path),
        Token::Symbol("foobar".to_owned()),
        Token::String("foo".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_application_complex_path_identifier_tokens_are_application_expression() -> TestResult
{
    let mut parser = Parser::default();
    let expected = quote! {
        ((foo::bar::foobar::barfoo))
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foo".to_owned()),
        Token::Operator(Operator::Path),
        Token::Symbol("bar".to_owned()),
        Token::Operator(Operator::Path),
        Token::Symbol("foobar".to_owned()),
        Token::Operator(Operator::Path),
        Token::Symbol("barfoo".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_static_module_tokens_are_module_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Module
        (0: static module foo () () ())
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Modifier(Modifier::Static),
        Token::Keyword(Keyword::Module),
        Token::Symbol("foo".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parsed_function_with_types_tokens_are_function_expressions() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Function
        (0: private eager function foo [(x: any) (y: Bar) (z: int...)] -> bool
            0: (bar 42))
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foo".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_owned()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Any),
        Token::Symbol("y".to_owned()),
        Token::Operator(Operator::Type),
        Token::Symbol("Bar".to_owned()),
        Token::Symbol("z".to_owned()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::PrimitiveType(PrimitiveType::Boolean),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("bar".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_parse_function_with_lambda_type() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote! {
        @node Function
        (0: private eager function foobar
            [(x: any) (y: (int bool -> Test)) (z: int...)] -> (-> int)
            0: (bar 42))
    };
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Function),
        Token::Symbol("foobar".to_owned()),
        Token::Parenthesis(Parenthesis::Open('[')),
        Token::Symbol("x".to_owned()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Any),
        Token::Symbol("y".to_owned()),
        Token::Operator(Operator::Type),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::PrimitiveType(PrimitiveType::Boolean),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::Symbol("Test".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Symbol("z".to_owned()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Operator(Operator::Variadic),
        Token::Parenthesis(Parenthesis::Close(']')),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Operator(Operator::SkinnyArrowRight),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("bar".to_owned()),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_lambda_type_is_parsed_correctly() -> TestResult {
    let parser = Parser::default();
    assert_eq!(
        quote!(@type (-> int)),
        parser.parse_type(
            &mut vec![
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')')),
            ]
            .into_iter()
            .peekable()
        )?,
    );
    assert_eq!(
        quote!(@type (int -> int)),
        parser.parse_type(
            &mut vec![
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')'))
            ]
            .into_iter()
            .peekable()
        )?,
    );
    assert_eq!(
        quote!(@type (int int -> int)),
        parser.parse_type(
            &mut vec![
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')'))
            ]
            .into_iter()
            .peekable()
        )?,
    );
    assert_eq!(
        quote!(@type ((int (int -> int)) -> int)),
        parser.parse_type(
            &mut vec![
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Open('(')),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')')),
                Token::Operator(Operator::SkinnyArrowRight),
                Token::PrimitiveType(PrimitiveType::Integer),
                Token::Parenthesis(Parenthesis::Close(')'))
            ]
            .into_iter()
            .peekable()
        )?,
    );
    Ok(())
}

#[test]
fn test_typed_let_is_let_expression() -> TestResult {
    let mut parser = Parser::default();
    let expected = quote!((let x: int 42 x));
    let actual = parser.parse(vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Keyword(Keyword::Let),
        Token::Symbol("x".to_owned()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Integer),
        Token::Number(NumberToken::Integer("42".to_owned())),
        Token::Symbol("x".to_owned()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ])?;
    assert_eq!(expected, actual);
    Ok(())
}
