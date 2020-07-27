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
fn test_comment_is_skipped() {
    let lexer = Lexer::default();
    let expected: Vec<Token> = Vec::new();
    let actual = lexer.tokenize("# test \t comment \t \n # foo bar");
    assert_eq!(expected, actual)
}

#[test]
fn test_tokenized_open_bracket_is_bracket_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Parenthesis(Parenthesis::Open('('))];
    let actual = lexer.tokenize("(");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_closing_bracket_is_bracket_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Parenthesis(Parenthesis::Close(')'))];
    let actual = lexer.tokenize(")");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_string_is_string_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::String("foo bar \" \\ foobar ".to_string())];
    let actual = lexer.tokenize("\"foo bar \\\" \\\\ foobar \"");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_single_character_number_is_number_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Number(7)];
    let actual = lexer.tokenize("7");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_multi_character_number_is_number_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Number(4224)];
    let actual = lexer.tokenize("4224");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_let_keyword_is_let_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Keyword(Keyword::Let)];
    let actual = lexer.tokenize("let");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_if_keyword_is_if_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Keyword(Keyword::If)];
    let actual = lexer.tokenize("if");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_single_character_symbol_is_symbol_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Symbol("x".to_string())];
    let actual = lexer.tokenize("x");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_multi_character_symbol_is_symbol_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Symbol("xyz".to_string())];
    let actual = lexer.tokenize("xyz");
    assert_eq!(expected, actual);
}

#[test]
fn test_tokenized_boolean_symbol_is_boolean_token() {
    let lexer = Lexer::default();
    let expected = vec![Token::Boolean(true), Token::Boolean(false)];
    let actual = lexer.tokenize("true false");
    assert_eq!(expected, actual);
}

#[test]
fn test_statement_is_tokenized_correctly() {
    let lexer = Lexer::default();
    let expected = vec![
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("multiply".to_string()),
        Token::Number(123),
        Token::Parenthesis(Parenthesis::Open('(')),
        Token::Symbol("+".to_string()),
        Token::Number(321),
        Token::Number(1),
        Token::Parenthesis(Parenthesis::Close(')')),
        Token::String("foobar".to_string()),
        Token::Parenthesis(Parenthesis::Close(')')),
    ];
    let actual = lexer.tokenize("(multiply 123 (+ 321 1) \"foobar\" )");
    assert_eq!(expected, actual);
}
