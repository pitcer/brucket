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
    Parameters,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Let,
    If,
    Lambda,
    Internal,
    Module,
    Function,
    Constant,
    And,
    Or,
    Letrec,
}

trait LexerCharacter {
    fn is_comment(&self) -> bool;

    fn is_opening_parenthesis(&self) -> bool;

    fn is_closing_parenthesis(&self) -> bool;

    fn is_parameter_parenthesis(&self) -> bool;

    fn is_section_break(&self) -> bool;

    fn is_quote(&self) -> bool;

    fn is_escaping(&self) -> bool;

    fn is_number(&self) -> bool;
}

impl LexerCharacter for char {
    fn is_comment(&self) -> bool {
        matches!(self, '#')
    }

    fn is_opening_parenthesis(&self) -> bool {
        matches!(self, '(' | '[' | '{')
    }

    fn is_closing_parenthesis(&self) -> bool {
        matches!(self, ')' | ']' | '}')
    }

    fn is_parameter_parenthesis(&self) -> bool {
        matches!(self, '|')
    }

    fn is_section_break(&self) -> bool {
        self.is_ascii_whitespace()
            || self.is_closing_parenthesis()
            || self.is_parameter_parenthesis()
    }

    fn is_quote(&self) -> bool {
        matches!(self, '"' | '\'')
    }

    fn is_escaping(&self) -> bool {
        matches!(self, '\\')
    }

    fn is_number(&self) -> bool {
        matches!(self, '0'..='9')
    }
}

type Characters<'a> = Peekable<Chars<'a>>;
type TokenResult = Result<Option<Token>, String>;

impl Default for Lexer {
    fn default() -> Self {
        let symbol_map = maplit::hashmap! {
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "let" => Token::Keyword(Keyword::Let),
            "letrec" => Token::Keyword(Keyword::Letrec),
            "if" => Token::Keyword(Keyword::If),
            "lambda" => Token::Keyword(Keyword::Lambda),
            "->" => Token::Keyword(Keyword::Lambda),
            "internal" => Token::Keyword(Keyword::Internal),
            "module" => Token::Keyword(Keyword::Module),
            "function" => Token::Keyword(Keyword::Function),
            "constant" => Token::Keyword(Keyword::Constant),
            "and" => Token::Keyword(Keyword::And),
            "or" => Token::Keyword(Keyword::Or),
        };
        Self { symbol_map }
    }
}

impl Lexer {
    pub fn tokenize(&self, syntax: &str) -> Result<Vec<Token>, String> {
        let mut result = Vec::new();
        let mut characters = syntax.chars().peekable();
        while let Some(current) = characters.next() {
            let token = self.match_token(current, &mut characters)?;
            if let Some(token) = token {
                result.push(token);
            }
        }
        Ok(result)
    }

    fn match_token(&self, current: char, characters: &mut Characters) -> TokenResult {
        match current {
            comment if comment.is_comment() => {
                Self::skip_comment(characters);
                Ok(None)
            }
            parenthesis if parenthesis.is_opening_parenthesis() => {
                Ok(Some(Token::Parenthesis(Parenthesis::Open(parenthesis))))
            }
            parenthesis if parenthesis.is_closing_parenthesis() => {
                Ok(Some(Token::Parenthesis(Parenthesis::Close(parenthesis))))
            }
            parenthesis if parenthesis.is_parameter_parenthesis() => {
                Ok(Some(Token::Parenthesis(Parenthesis::Parameters)))
            }
            quote if quote.is_quote() => {
                let string = Self::tokenize_string(characters);
                Ok(Some(Token::String(string)))
            }
            number if number.is_number() => {
                let number = Self::tokenize_number(current, characters)?;
                Ok(Some(Token::Number(number)))
            }
            whitespace if whitespace.is_ascii_whitespace() => Ok(None),
            _ => {
                let symbol = Self::tokenize_symbol(current, characters);
                let token = self.symbol_map.get(symbol.as_str());
                if let Some(token) = token {
                    Ok(Some(token.clone()))
                } else {
                    Ok(Some(Token::Symbol(symbol)))
                }
            }
        }
    }

    fn skip_comment(characters: &mut Characters) {
        for current in characters {
            if current == '\n' {
                return;
            }
        }
    }

    fn tokenize_string(characters: &mut Characters) -> String {
        let mut result = String::new();
        let mut escaping = false;
        for current in characters {
            if !escaping && current.is_escaping() {
                escaping = !escaping;
            } else if !escaping && current.is_quote() {
                break;
            } else {
                result.push(current);
                escaping = false;
            }
        }
        result
    }

    fn tokenize_number(first: char, characters: &mut Characters) -> Result<u32, String> {
        let mut result = first.to_digit(10).unwrap();
        while let Some(current) = characters.peek() {
            if current.is_section_break() {
                break;
            }
            if current.is_number() {
                result = result * 10 + current.to_digit(10).unwrap();
            } else {
                return Err("Invalid number character".to_string());
            }
            characters.next();
        }
        Ok(result)
    }

    fn tokenize_symbol(first: char, characters: &mut Characters) -> String {
        let mut result = String::new();
        result.push(first);
        while let Some(current) = characters.peek() {
            if current.is_section_break() {
                break;
            }
            result.push(*current);
            characters.next();
        }
        result
    }
}

#[cfg(test)]
mod test;
