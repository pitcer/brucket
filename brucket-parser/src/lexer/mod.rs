use crate::token::{
    Boolean, Keyword, Modifier, Number, Operator, Parenthesis, PrimitiveType, Token,
};
use derive_more::Constructor;
use std::borrow::Cow;
use std::iter::Peekable;
use std::option::Option::Some;
use std::str::Chars;

#[cfg(test)]
mod tests;

#[derive(Constructor)]
pub struct Lexer;

trait LexerCharacter {
    fn is_comment(&self) -> bool;

    fn is_opening_parenthesis(&self) -> bool;

    fn is_closing_parenthesis(&self) -> bool;

    fn is_section_break(&self) -> bool;

    fn is_number_break(&self) -> bool;

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
        self.is_number_break() || matches!(self, '.' | ':')
    }

    fn is_number_break(&self) -> bool {
        self.is_ascii_whitespace() || self.is_opening_parenthesis() || self.is_closing_parenthesis()
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
type TokenError = Cow<'static, str>;
type TokenResult = Result<Option<Token>, TokenError>;

impl Lexer {
    #[inline]
    #[allow(clippy::unused_self)]
    pub fn tokenize(&self, syntax: &str) -> Result<Vec<Token>, TokenError> {
        let mut result = Vec::new();
        let mut characters = syntax.chars().peekable();
        while let Some(current) = characters.next() {
            let token = Self::match_token(current, &mut characters)?;
            if let Some(token) = token {
                result.push(token);
            }
        }
        Ok(result)
    }

    fn match_token(current: char, characters: &mut Characters<'_>) -> TokenResult {
        match current {
            comment if comment.is_comment() => {
                Self::skip_comment(characters);
                Ok(None)
            }
            '.' => Self::tokenize_dots(characters),
            ':' => Ok(Some(Self::tokenize_colon(characters))),
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
                let number = Self::tokenize_number(current, characters);
                Ok(Some(Token::Number(number)))
            }
            whitespace if whitespace.is_ascii_whitespace() => Ok(None),
            _ => {
                let symbol = Self::tokenize_symbol(current, characters);
                let token = Self::match_symbol_with_token(symbol);
                Ok(Some(token))
            }
        }
    }

    fn match_symbol_with_token(symbol: String) -> Token {
        match symbol.as_str() {
            "true" => Token::Boolean(Boolean::True),
            "false" => Token::Boolean(Boolean::False),
            "null" => Token::Null,
            "let" => Token::Keyword(Keyword::Let),
            "if" => Token::Keyword(Keyword::If),
            "lambda" => Token::Keyword(Keyword::Lambda),
            "module" => Token::Keyword(Keyword::Module),
            "function" | "fun" => Token::Keyword(Keyword::Function),
            "constant" | "const" => Token::Keyword(Keyword::Constant),
            "->" => Token::Operator(Operator::SkinnyArrowRight),
            "=>" => Token::Operator(Operator::ThickArrowRight),
            "public" | "pub" => Token::Modifier(Modifier::Public),
            "private" => Token::Modifier(Modifier::Private),
            "lazy" => Token::Modifier(Modifier::Lazy),
            "static" => Token::Modifier(Modifier::Static),
            "internal" => Token::Modifier(Modifier::Internal),
            "boo" => Token::PrimitiveType(PrimitiveType::Boolean),
            "int" => Token::PrimitiveType(PrimitiveType::Integer),
            "flo" => Token::PrimitiveType(PrimitiveType::Float),
            "str" => Token::PrimitiveType(PrimitiveType::String),
            "any" => Token::PrimitiveType(PrimitiveType::Any),
            "uni" => Token::PrimitiveType(PrimitiveType::Unit),
            _ => Token::Symbol(symbol),
        }
    }

    fn skip_comment(characters: &mut Characters<'_>) {
        for current in characters {
            if current == '\n' {
                return;
            }
        }
    }

    fn tokenize_dots(characters: &mut Characters<'_>) -> TokenResult {
        let second = characters.next();
        let third = characters.next();
        second
            .filter(|second| *second == '.')
            .and(third)
            .filter(|third| *third == '.')
            .map_or_else(
                || Err(Cow::from("Invalid dots operator")),
                |_| Ok(Some(Token::Operator(Operator::Variadic))),
            )
    }

    fn tokenize_colon(characters: &mut Characters<'_>) -> Token {
        let second = characters.peek();
        if let Some(':') = second {
            characters.next();
            return Token::Operator(Operator::Path);
        }
        Token::Operator(Operator::Type)
    }

    fn tokenize_string(characters: &mut Characters<'_>) -> String {
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

    fn tokenize_number(first: char, characters: &mut Characters<'_>) -> Number {
        let mut number = String::new();
        let mut floating_point = false;
        number.push(first);
        while let Some(current) = characters.peek() {
            if current.is_number_break() {
                break;
            }
            if *current == '.' {
                floating_point = true;
            }
            number.push(*current);
            characters.next();
        }
        if floating_point {
            Number::FloatingPoint(number)
        } else {
            Number::Integer(number)
        }
    }

    fn tokenize_symbol(first: char, characters: &mut Characters<'_>) -> String {
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
