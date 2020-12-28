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
use std::option::Option::Some;
use std::str::Chars;

use crate::token::{Keyword, Modifier, Operator, Parenthesis, PrimitiveType, Token};

#[cfg(test)]
mod test;

pub struct Lexer {
    symbol_map: HashMap<&'static str, Token>,
}

trait LexerCharacter {
    fn is_comment(&self) -> bool;

    fn is_opening_parenthesis(&self) -> bool;

    fn is_closing_parenthesis(&self) -> bool;

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

    fn is_section_break(&self) -> bool {
        self.is_ascii_whitespace()
            || self.is_opening_parenthesis()
            || self.is_closing_parenthesis()
            || matches!(self, '.' | ':')
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
            "null" => Token::Null,
            "let" => Token::Keyword(Keyword::Let),
            "if" => Token::Keyword(Keyword::If),
            "lambda" => Token::Keyword(Keyword::Lambda),
            "internal" => Token::Keyword(Keyword::Internal),
            "module" => Token::Keyword(Keyword::Module),
            "function" => Token::Keyword(Keyword::Function),
            "fun" => Token::Keyword(Keyword::Function),
            "constant" => Token::Keyword(Keyword::Constant),
            "const" => Token::Keyword(Keyword::Constant),
            "->" => Token::Operator(Operator::SkinnyArrowRight),
            "=>" => Token::Operator(Operator::ThickArrowRight),
            "public" => Token::Modifier(Modifier::Public),
            "pub" => Token::Modifier(Modifier::Public),
            "private" => Token::Modifier(Modifier::Private),
            "lazy" => Token::Modifier(Modifier::Lazy),
            "static" => Token::Modifier(Modifier::Static),
            "boo" => Token::PrimitiveType(PrimitiveType::Boolean),
            "int" => Token::PrimitiveType(PrimitiveType::Integer),
            "str" => Token::PrimitiveType(PrimitiveType::String),
            "any" => Token::PrimitiveType(PrimitiveType::Any),
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
            '.' => Self::tokenize_dots(characters),
            ':' => Self::tokenize_colon(characters),
            parenthesis if parenthesis.is_opening_parenthesis() => {
                Ok(Some(Token::Parenthesis(Parenthesis::Open(parenthesis))))
            }
            parenthesis if parenthesis.is_closing_parenthesis() => {
                Ok(Some(Token::Parenthesis(Parenthesis::Close(parenthesis))))
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

    fn tokenize_dots(characters: &mut Characters) -> Result<Option<Token>, String> {
        let second = characters.next();
        let third = characters.next();
        if second.is_some() && second.unwrap() == '.' && third.is_some() && third.unwrap() == '.' {
            Ok(Some(Token::Operator(Operator::Variadic)))
        } else {
            Err("Invalid dots operator".to_string())
        }
    }

    fn tokenize_colon(characters: &mut Characters) -> Result<Option<Token>, String> {
        let second = characters.peek();
        if let Some(second) = second {
            if let ':' = second {
                characters.next();
                return Ok(Some(Token::Operator(Operator::Path)));
            }
        }
        Ok(Some(Token::Operator(Operator::Type)))
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
