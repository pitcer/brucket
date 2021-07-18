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

use crate::evaluator::ValueResult;
use crate::value::Value;
use std::borrow::Cow;
use std::collections::HashMap;

pub fn new(mut environment: HashMap<String, Value>) -> ValueResult {
    let first = environment
        .remove("first")
        .ok_or("[pair_new] Unknown variable: first")?;
    let second = environment
        .remove("second")
        .ok_or("[pair_new] Unknown variable: second")?;
    Ok(Value::Pair(Box::new(first), Box::new(second)))
}

pub fn first(mut environment: HashMap<String, Value>) -> ValueResult {
    let pair = environment
        .remove("pair")
        .ok_or("[pair_first] Unknown variable: pair")?;
    if let Value::Pair(first, _) = pair {
        Ok(*first)
    } else {
        Err(Cow::from("Invalid type of argument, expected: Pair"))
    }
}

pub fn second(mut environment: HashMap<String, Value>) -> ValueResult {
    let pair = environment
        .remove("pair")
        .ok_or("[pair_second] Unknown variable: pair")?;
    if let Value::Pair(_, second) = pair {
        Ok(*second)
    } else {
        Err(Cow::from("Invalid type of argument, expected: Pair"))
    }
}
