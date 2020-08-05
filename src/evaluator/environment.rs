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

use crate::evaluator::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::FromIterator;
use std::option::Option::Some;
use std::rc::{Rc, Weak};

#[cfg(test)]
macro_rules! environment {
    ($($identifier:expr => $value:expr),*) => {
        {
            let environment = Environment::default();
            $(
                environment.insert(
                    $identifier.to_string(),
                    std::rc::Rc::new($value)
                );
            )*
            environment
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    map: RefCell<HashMap<String, Rc<Value>>>,
    weak_map: RefCell<HashMap<String, WeakWrapper<Value>>>,
}

#[derive(Debug, Clone)]
struct WeakWrapper<T> {
    weak: Weak<T>,
}

impl<T: PartialEq> PartialEq for WeakWrapper<T> {
    fn eq(&self, other: &Self) -> bool {
        let this = self.upgrade();
        if let Some(this) = this {
            let that = other.upgrade();
            if let Some(that) = that {
                return this == that;
            }
        }
        false
    }
}

impl<T> WeakWrapper<T> {
    fn new(weak: Weak<T>) -> Self {
        Self { weak }
    }

    fn clone(&self) -> Weak<T> {
        Weak::clone(&self.weak)
    }

    fn upgrade(&self) -> Option<Rc<T>> {
        Weak::upgrade(&self.weak)
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::from_map(HashMap::default())
    }
}

impl FromIterator<(String, Rc<Value>)> for Environment {
    fn from_iter<T: IntoIterator<Item = (String, Rc<Value>)>>(iter: T) -> Self {
        let map = HashMap::from_iter(iter);
        Self::from_map(map)
    }
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    fn from_map(map: HashMap<String, Rc<Value>>) -> Self {
        let map = RefCell::new(map);
        let weak_map = RefCell::new(HashMap::new());
        Self { map, weak_map }
    }

    pub fn insert(&self, key: String, value: Rc<Value>) {
        self.map.borrow_mut().insert(key, value);
    }

    pub fn insert_weak(&self, key: String, value: Weak<Value>) {
        self.weak_map
            .borrow_mut()
            .insert(key, WeakWrapper::new(value));
    }

    pub fn insert_all_weak(&self, other: &Self) {
        for (key, value) in other.weak_map.borrow().iter() {
            self.insert_weak(key.clone(), WeakWrapper::clone(value));
        }
    }

    pub fn get(&self, key: &str) -> Option<Rc<Value>> {
        self.map.borrow().get(key).map(|value| Rc::clone(value))
    }

    pub fn get_weak(&self, key: &str) -> Option<Weak<Value>> {
        self.weak_map
            .borrow()
            .get(key)
            .map(|value| WeakWrapper::clone(value))
    }

    pub fn remove(&mut self, key: &str) {
        self.map.borrow_mut().remove(key);
    }
}
