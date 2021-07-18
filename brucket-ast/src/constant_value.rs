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

use crate::NodeId;
use derive_more::Constructor;

#[derive(Debug, Clone, PartialEq, Constructor)]
pub struct ConstantValue {
    pub node_id: NodeId,
    pub variant: ConstantVariant,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantVariant {
    Unit,
    Null,
    Numeric(Number),
    Boolean(Boolean),
    String(String),
}

macro_rules! impl_from_number_for_constant_variant {
    ($($number_type:ty)+) => {
        $(
            impl From<$number_type> for ConstantVariant {
                fn from(number: $number_type) -> Self {
                    ConstantVariant::Numeric(Number::from(number))
                }
            }
        )+
    };
}

impl_from_number_for_constant_variant!(usize u8 u16 u32 u64 isize i8 i16 i32 i64 f32 f64);

impl From<bool> for ConstantVariant {
    fn from(boolean: bool) -> Self {
        ConstantVariant::Boolean(Boolean::from(boolean))
    }
}

impl From<&str> for ConstantVariant {
    fn from(string: &str) -> Self {
        Self::from(string.to_owned())
    }
}

impl From<String> for ConstantVariant {
    fn from(string: String) -> Self {
        ConstantVariant::String(string)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Integer(String),
    FloatingPoint(String),
}

macro_rules! impl_from_for_number {
    ($identifier:ident $($number_type:ty)+) => {
        $(
            impl From<$number_type> for Number {
                fn from(number: $number_type) -> Self {
                    Number::$identifier(number.to_string())
                }
            }
        )+
    };
}

impl_from_for_number!(Integer usize u8 u16 u32 u64 isize i8 i16 i32 i64);
impl_from_for_number!(FloatingPoint f32 f64);

#[derive(Debug, Clone, PartialEq)]
pub enum Boolean {
    True,
    False,
}

impl From<bool> for Boolean {
    fn from(boolean: bool) -> Self {
        if boolean {
            Boolean::True
        } else {
            Boolean::False
        }
    }
}
