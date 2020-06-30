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

use std::slice::Iter;

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum Expression {
    Constant(u32),
    Symbol(String),
    Function(Box<Expression>, Vec<Expression>)
}

pub fn parse(tokens: &Vec<Token>) -> Result<Expression, String> {
    let mut iterator = tokens.iter();
    parse_iterator(&mut iterator)
}

fn parse_iterator(tokens_iterator: &mut Iter<Token>) -> Result<Expression, String> {
    let mut function_name: Option<Box<Expression>> = None;
    let mut arguments: Vec<Expression> = Vec::new();
    let mut current;
    loop {
        let next = tokens_iterator.next();
        if next.is_none() {
            return Ok(arguments[0].clone());
        }
        current = next.unwrap();
        match current {
            Token::Parenthesis('(') => arguments.push(parse_iterator(tokens_iterator)?),
            Token::Parenthesis(')') => return Ok(Expression::Function(function_name.expect("No function name"), arguments)),
            Token::Number(number) => arguments.push(Expression::Constant(*number)),
            Token::Symbol(symbol) => {
                if function_name.is_none() {
                    function_name = Some(Box::new(Expression::Symbol(symbol.clone())))
                } else {
                    arguments.push(Expression::Symbol(symbol.clone()));
                }
            }
            _ => {}
        }
    }
}
