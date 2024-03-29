use crate::value::Value;
use derive_more::Constructor;
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
                    $identifier.to_owned(),
                    std::rc::Rc::new($value)
                );
            )*
            environment
        }
    };
}

#[derive(Debug, Clone, Constructor)]
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
    fn clone(&self) -> Weak<T> {
        Weak::clone(&self.weak)
    }

    fn upgrade(&self) -> Option<Rc<T>> {
        Weak::upgrade(&self.weak)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    map: RefCell<HashMap<String, Rc<Value>>>,
    weak_map: RefCell<HashMap<String, WeakWrapper<Value>>>,
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
    fn from_map(map: HashMap<String, Rc<Value>>) -> Self {
        let map = RefCell::new(map);
        let weak_map = RefCell::new(HashMap::new());
        Self { map, weak_map }
    }

    pub fn insert(&self, key: String, value: Rc<Value>) {
        self.map.borrow_mut().insert(key, value);
    }

    pub fn insert_all(&self, other: &Self) {
        for (key, value) in other.map.borrow().iter() {
            self.insert(key.clone(), Rc::clone(value));
        }
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

    pub fn remove(&self, key: &str) {
        self.map.borrow_mut().remove(key);
    }
}
