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

type TestResult = Result<(), Cow<'static, str>>;

#[test]
fn test_comment_is_skipped() -> TestResult {
    let lexer = Lexer::default();
    let expected: Vec<Token> = Vec::new();
    let actual = lexer.tokenize("# test \t comment \t \n # foo bar".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_open_parenthesis_is_parenthesis_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Parenthesis(Parenthesis::Open('('))];
    let actual = lexer.tokenize("(".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_close_parenthesis_is_parenthesis_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Parenthesis(Parenthesis::Close(')'))];
    let actual = lexer.tokenize(")".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_open_square_bracket_is_parenthesis_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Parenthesis(Parenthesis::Open('['))];
    let actual = lexer.tokenize("[".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_close_square_bracket_is_parenthesis_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Parenthesis(Parenthesis::Close(']'))];
    let actual = lexer.tokenize("]".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_string_is_string_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::String("foo bar \" \\ foobar ".to_string())];
    let actual = lexer.tokenize("\"foo bar \\\" \\\\ foobar \"".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_single_character_number_is_number_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Number(Number::Integer("7".to_string()))];
    let actual = lexer.tokenize("7".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_multi_character_number_is_number_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Number(Number::Integer("4224".to_string()))];
    let actual = lexer.tokenize("4224".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_single_character_symbol_is_symbol_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Symbol("x".to_string())];
    let actual = lexer.tokenize("x".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_multi_character_symbol_is_symbol_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Symbol("xyz".to_string())];
    let actual = lexer.tokenize("xyz".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_null_is_null_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Null];
    let actual = lexer.tokenize("null".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_boolean_symbol_is_boolean_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![
        Token::Boolean(Boolean::True),
        Token::Boolean(Boolean::False),
    ];
    let actual = lexer.tokenize("true false".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_let_keyword_is_let_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Keyword(Keyword::Let)];
    let actual = lexer.tokenize("let".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_if_keyword_is_if_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Keyword(Keyword::If)];
    let actual = lexer.tokenize("if".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_lambda_keyword_is_lambda_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Keyword(Keyword::Lambda)];
    let actual = lexer.tokenize("lambda".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_skinny_right_arrow_symbol_is_skinny_arrow_right_operator() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Operator(Operator::SkinnyArrowRight)];
    let actual = lexer.tokenize("->".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_thick_right_arrow_symbol_is_thick_arrow_right_operator() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Operator(Operator::ThickArrowRight)];
    let actual = lexer.tokenize("=>".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_internal_modifier_is_internal_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Modifier(Modifier::Internal)];
    let actual = lexer.tokenize("internal".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_module_keyword_is_module_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Keyword(Keyword::Module)];
    let actual = lexer.tokenize("module".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_function_keyword_is_function_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Keyword(Keyword::Function)];
    let actual = lexer.tokenize("function".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_constant_keyword_is_constant_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Keyword(Keyword::Constant)];
    let actual = lexer.tokenize("constant".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_public_modifier_is_public_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Modifier(Modifier::Public)];
    let actual = lexer.tokenize("public".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_private_modifier_is_private_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Modifier(Modifier::Private)];
    let actual = lexer.tokenize("private".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_lazy_modifier_is_lazy_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Modifier(Modifier::Lazy)];
    let actual = lexer.tokenize("lazy".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_static_modifier_is_static_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Modifier(Modifier::Static)];
    let actual = lexer.tokenize("static".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_statement_is_tokenized_correctly() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("multiply".to_string()),
        Token::Number(Number::Integer("123".to_string())),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("+".to_string()),
        Token::Number(Number::Integer("321".to_string())),
        Token::Number(Number::Integer("1".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::String("foobar".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ];
    let actual = lexer.tokenize("(multiply 123 (+ 321 1) \"foobar\" )".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_closing_parenthesis_after_number_is_handled_correctly() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Number(Number::Integer("42".to_string())),
        Token::Parenthesis(Parenthesis::Close(')')),
    ];
    let actual = lexer.tokenize("(42)".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_closing_parenthesis_after_symbol_is_handled_correctly() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("foo".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ];
    let actual = lexer.tokenize("(foo)".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenize_number() {
    let mut characters = "224)".chars().peekable();
    let number = Lexer::tokenize_number('4', &mut characters);
    let parenthesis = characters.next().expect("Missing closing parenthesis");
    assert_eq!(Number::Integer("4224".to_string()), number);
    assert_eq!(')', parenthesis);
}

#[test]
fn test_tokenize_floating_point_number() {
    let mut characters = "2.24)".chars().peekable();
    let number = Lexer::tokenize_number('4', &mut characters);
    let parenthesis = characters.next().expect("Missing closing parenthesis");
    assert_eq!(Number::FloatingPoint("42.24".to_string()), number);
    assert_eq!(')', parenthesis);
}

#[test]
fn test_tokenize_symbol() {
    let mut characters = "oo)".chars().peekable();
    let symbol = Lexer::tokenize_symbol('f', &mut characters);
    let parenthesis = characters.next().expect("Missing closing parenthesis");
    assert_eq!("foo".to_string(), symbol);
    assert_eq!(')', parenthesis);
}

#[test]
fn test_tokenized_three_dots_are_variadic_operator_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Operator(Operator::Variadic)];
    let actual = lexer.tokenize("...".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_colon_is_type_operator_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Operator(Operator::Type)];
    let actual = lexer.tokenize(":".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_two_colons_are_path_operator_token() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![Token::Operator(Operator::Path)];
    let actual = lexer.tokenize("::".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_path_is_path_tokens() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![
        Token::Symbol("foo".to_string()),
        Token::Operator(Operator::Path),
        Token::Symbol("bar".to_string()),
    ];
    let actual = lexer.tokenize("foo::bar".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_tokenized_complex_path_is_path_tokens() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![
        Token::Symbol("foo".to_string()),
        Token::Operator(Operator::Path),
        Token::Symbol("bar".to_string()),
        Token::Operator(Operator::Path),
        Token::Symbol("foobar".to_string()),
        Token::Operator(Operator::Path),
        Token::Symbol("barfoo".to_string()),
    ];
    let actual = lexer.tokenize("foo::bar::foobar::barfoo".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_typed_variadic_parameter_is_tokenized_correctly() -> TestResult {
    let lexer = Lexer::default();
    let expected = vec![
        Token::Symbol("foo".to_string()),
        Token::Operator(Operator::Type),
        Token::PrimitiveType(PrimitiveType::Any),
        Token::Operator(Operator::Variadic),
    ];
    let actual = lexer.tokenize("foo:any...".into())?;
    assert_eq!(expected, actual);
    let actual = lexer.tokenize("foo: any ...".into())?;
    assert_eq!(expected, actual);
    let actual = lexer.tokenize("foo : any...".into())?;
    assert_eq!(expected, actual);
    let actual = lexer.tokenize("foo :any ...".into())?;
    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn test_primitive_types_are_tokenized_correctly() -> TestResult {
    let lexer = Lexer::default();
    assert_eq!(
        vec![Token::PrimitiveType(PrimitiveType::Boolean)],
        lexer.tokenize("boo".into())?
    );
    assert_eq!(
        vec![Token::PrimitiveType(PrimitiveType::Integer)],
        lexer.tokenize("int".into())?
    );
    assert_eq!(
        vec![Token::PrimitiveType(PrimitiveType::Float)],
        lexer.tokenize("flo".into())?
    );
    assert_eq!(
        vec![Token::PrimitiveType(PrimitiveType::String)],
        lexer.tokenize("str".into())?
    );
    assert_eq!(
        vec![Token::PrimitiveType(PrimitiveType::Any)],
        lexer.tokenize("any".into())?
    );
    assert_eq!(
        vec![Token::PrimitiveType(PrimitiveType::Unit)],
        lexer.tokenize("uni".into())?
    );
    Ok(())
}

#[test]
fn test_lambda_type_is_tokenized_correctly() -> TestResult {
    let lexer = Lexer::default();
    assert_eq!(
        vec![
            Token::Parenthesis(Parenthesis::Open('(')),
            Token::Operator(Operator::SkinnyArrowRight),
            Token::PrimitiveType(PrimitiveType::Integer),
            Token::Parenthesis(Parenthesis::Close(')')),
        ],
        lexer.tokenize("(-> int)".into())?
    );
    assert_eq!(
        vec![
            Token::Parenthesis(Parenthesis::Open('(')),
            Token::PrimitiveType(PrimitiveType::Integer),
            Token::Operator(Operator::SkinnyArrowRight),
            Token::PrimitiveType(PrimitiveType::Integer),
            Token::Parenthesis(Parenthesis::Close(')'))
        ],
        lexer.tokenize("(int -> int)".into())?
    );
    assert_eq!(
        vec![
            Token::Parenthesis(Parenthesis::Open('(')),
            Token::PrimitiveType(PrimitiveType::Integer),
            Token::PrimitiveType(PrimitiveType::Integer),
            Token::Operator(Operator::SkinnyArrowRight),
            Token::PrimitiveType(PrimitiveType::Integer),
            Token::Parenthesis(Parenthesis::Close(')'))
        ],
        lexer.tokenize("(int int -> int)".into())?
    );
    assert_eq!(
        vec![
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
        ],
        lexer.tokenize("(int (int -> int) -> int)".into())?
    );
    Ok(())
}
