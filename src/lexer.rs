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

use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer {
    symbol_map: HashMap<&'static str, Token>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Parenthesis(Parenthesis),
    String(String),
    Number(u32),
    Boolean(bool),
    Keyword(Keyword),
    Symbol(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Parenthesis {
    Open(char),
    Close(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Let,
}

impl Lexer {
    pub fn default() -> Lexer {
        let symbol_map = maplit::hashmap! {
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "let" => Token::Keyword(Keyword::Let),
        };
        Lexer { symbol_map }
    }

    pub fn tokenize(&self, syntax: &str) -> Vec<Token> {
        let mut result = Vec::new();
        let mut chars = syntax.chars().peekable();
        let mut next = chars.next();
        while next.is_some() {
            let current = next.unwrap();
            let token = self.match_token(&mut chars, current);
            if let Some(token) = token {
                result.push(token);
            }
            next = chars.next();
        }
        result
    }

    fn match_token(&self, chars: &mut Peekable<Chars>, current: char) -> Option<Token> {
        match current {
            '#' => {
                Lexer::skip_comment(chars);
                None
            }
            '(' | '[' | '{' => Some(Token::Parenthesis(Parenthesis::Open(current))),
            ')' | ']' | '}' => Some(Token::Parenthesis(Parenthesis::Close(current))),
            '"' => Some(Token::String(Lexer::tokenize_string(chars))),
            '0'..='9' => Some(Token::Number(Lexer::tokenize_number(chars, current))),
            ' ' | '\n' | '\t' | '\r' => None,
            _ => {
                let symbol = Lexer::tokenize_symbol(chars, current);
                let token = self.symbol_map.get(symbol.as_str());
                if let Some(token) = token {
                    Some(token.clone())
                } else {
                    Some(Token::Symbol(symbol))
                }
            }
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
}

#[cfg(test)]
mod test {
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
}
