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
    Parenthesis(char),
    Number(u32),
    Symbol(String),
}

pub fn tokenize(syntax: &String) -> Result<Vec<Token>, String> {
    let mut result = Vec::new();
    let mut chars = syntax.chars().peekable();
    let mut current;
    loop {
        let next = chars.next();
        if next.is_none() {
            return Ok(result);
        }
        current = next.unwrap();
        match current {
            '(' | ')' => result.push(Token::Parenthesis(current)),
            '0'..='9' => result.push(Token::Number(tokenize_number(&mut chars, current)?)),
            'A'..='Z' | 'a'..='z' | '_' | '+' | '-' | '*' | '/' | '%' => {
                result.push(Token::Symbol(tokenize_symbol(&mut chars, current)?))
            }
            _ => (),
        }
    }
}

fn tokenize_number(chars: &mut Peekable<Chars>, last: char) -> Result<u32, String> {
    let mut result = last.to_digit(10).expect("Invalid digit");
    let mut current;
    loop {
        let next = chars.peek();
        if next.is_none() {
            return Ok(result);
        }
        current = next.unwrap();
        match current {
            '0'..='9' => result = result * 10 + current.to_digit(10).expect("Invalid digit"),
            ' ' | ')' => return Ok(result),
            _ => (),
        }
        chars.next();
    }
}

fn tokenize_symbol(chars: &mut Peekable<Chars>, last: char) -> Result<String, String> {
    let mut result = String::new();
    result.push(last);
    let mut current;
    loop {
        let next = chars.peek();
        if next.is_none() {
            return Ok(result);
        }
        current = next.unwrap();
        match current {
            ' ' | ')' => return Ok(result),
            _ => result.push(*current),
        }
        chars.next();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenized_open_bracket_is_bracket_token() -> Result<(), String> {
        let expected = vec![Token::Parenthesis('(')];
        let actual = tokenize(&"(".to_string())?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_tokenized_closing_bracket_is_bracket_token() -> Result<(), String> {
        let expected = vec![Token::Parenthesis(')')];
        let actual = tokenize(&")".to_string())?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_tokenized_single_character_number_is_number_token() -> Result<(), String> {
        let expected = vec![Token::Number(7)];
        let actual = tokenize(&"7".to_string())?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_tokenized_multi_character_number_is_number_token() -> Result<(), String> {
        let expected = vec![Token::Number(4224)];
        let actual = tokenize(&"4224".to_string())?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_tokenized_single_character_symbol_is_symbol_token() -> Result<(), String> {
        let expected = vec![Token::Symbol("x".to_string())];
        let actual = tokenize(&"x".to_string())?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_tokenized_multi_character_symbol_is_symbol_token() -> Result<(), String> {
        let expected = vec![Token::Symbol("xyz".to_string())];
        let actual = tokenize(&"xyz".to_string())?;
        assert_eq!(expected, actual);
        Ok(())
    }

    #[test]
    fn test_statement_is_tokenized_correctly() -> Result<(), String> {
        let expected = vec![
            Token::Parenthesis('('),
            Token::Symbol("multiply".to_string()),
            Token::Number(123),
            Token::Parenthesis('('),
            Token::Symbol("+".to_string()),
            Token::Number(321),
            Token::Number(1),
            Token::Parenthesis(')'),
            Token::Parenthesis(')'),
        ];
        let actual = tokenize(&"(multiply 123 (+ 321 1))".to_string())?;
        assert_eq!(expected, actual);
        Ok(())
    }
}
