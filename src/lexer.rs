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

use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum Token {
    Parenthesis(Parenthesis),
    String(String),
    Number(u32),
    Symbol(String),
}

#[derive(Debug, PartialEq)]
pub enum Parenthesis {
    Open(char),
    Close(char),
}

pub fn tokenize(syntax: &str) -> Vec<Token> {
    let mut result = Vec::new();
    let mut chars = syntax.chars().peekable();
    let mut next = chars.next();
    while next.is_some() {
        let current = next.unwrap();
        let token = match_token(&mut chars, current);
        if let Some(token) = token {
            result.push(token);
        }
        next = chars.next();
    }
    result
}

fn match_token(chars: &mut Peekable<Chars>, current: char) -> Option<Token> {
    match current {
        '#' => {
            skip_comment(chars);
            None
        }
        '(' | '[' | '{' => Some(Token::Parenthesis(Parenthesis::Open(current))),
        ')' | ']' | '}' => Some(Token::Parenthesis(Parenthesis::Close(current))),
        '"' => Some(Token::String(tokenize_string(chars))),
        '0'..='9' => Some(Token::Number(tokenize_number(chars, current))),
        ' ' | '\n' | '\t' | '\r' => None,
        _ => Some(Token::Symbol(tokenize_symbol(chars, current))),
    }
}

fn skip_comment(chars: &mut Peekable<Chars>) {
    let mut next = chars.next();
    while next.is_some() {
        let current = next.unwrap();
        if current == '\n' {
            return;
        }
        next = chars.next();
    }
}

fn tokenize_string(chars: &mut Peekable<Chars>) -> String {
    let mut result = String::new();
    let mut next = chars.next();
    let mut escaping = false;
    while next.is_some() {
        let current = next.unwrap();
        if !escaping && current == '\\' {
            escaping = !escaping;
        } else if !escaping && current == '"' {
            break;
        } else {
            result.push(current);
            escaping = false;
        }
        next = chars.next();
    }
    result
}

fn tokenize_number(chars: &mut Peekable<Chars>, last: char) -> u32 {
    let mut result = last.to_digit(10).expect("Invalid digit");
    let mut current;
    loop {
        let next = chars.peek();
        if next.is_none() {
            return result;
        }
        current = next.unwrap();
        match current {
            '0'..='9' => result = result * 10 + current.to_digit(10).expect("Invalid digit"),
            ' ' | '\n' | '\t' | '\r' | ')' | ']' | '}' => return result,
            _ => (),
        }
        chars.next();
    }
}

fn tokenize_symbol(chars: &mut Peekable<Chars>, last: char) -> String {
    let mut result = String::new();
    result.push(last);
    let mut current;
    loop {
        let next = chars.peek();
        if next.is_none() {
            return result;
        }
        current = next.unwrap();
        match current {
            ' ' | '\n' | '\t' | '\r' | ')' | ']' | '}' => return result,
            _ => result.push(*current),
        }
        chars.next();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_comment_is_skipped() {
        let expected: Vec<Token> = Vec::new();
        let actual = tokenize("# test \t comment \t \n # foo bar");
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_tokenized_open_bracket_is_bracket_token() {
        let expected = vec![Token::Parenthesis(Parenthesis::Open('('))];
        let actual = tokenize("(");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenized_closing_bracket_is_bracket_token() {
        let expected = vec![Token::Parenthesis(Parenthesis::Close(')'))];
        let actual = tokenize(")");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenized_string_is_string_token() {
        let expected = vec![Token::String("foo bar \" \\ foobar ".to_string())];
        let actual = tokenize("\"foo bar \\\" \\\\ foobar \"");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenized_single_character_number_is_number_token() {
        let expected = vec![Token::Number(7)];
        let actual = tokenize("7");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenized_multi_character_number_is_number_token() {
        let expected = vec![Token::Number(4224)];
        let actual = tokenize("4224");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenized_single_character_symbol_is_symbol_token() {
        let expected = vec![Token::Symbol("x".to_string())];
        let actual = tokenize("x");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenized_multi_character_symbol_is_symbol_token() {
        let expected = vec![Token::Symbol("xyz".to_string())];
        let actual = tokenize("xyz");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_statement_is_tokenized_correctly() {
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
        let actual = tokenize("(multiply 123 (+ 321 1) \"foobar\" )");
        assert_eq!(expected, actual);
    }
}
