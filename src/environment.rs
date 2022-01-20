use super::parser::Literal;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Environment {
    enclosing: Option<Box<RefCell<Environment>>>,
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new(enclosing: Option<Box<RefCell<Environment>>>) -> Self {
        Self {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Literal> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.enclosing {
                Some(enclosing) => match enclosing.borrow().get(name) {
                    Some(value) => Some(value.clone()),
                    None => None,
                },
                None => None,
            },
        }
    }

    pub fn define(&mut self, name: &str, value: Literal) -> Result<(), ()> {
        match self.values.insert(String::from(name), value) {
            Some(_) => Err(()),
            None => Ok(()),
        }
    }

    pub fn assign(&mut self, name: &str, value: &Literal) -> Result<(), ()> {
        if self.values.contains_key(name) {
            self.values.insert(String::from(name), value.clone());
            Ok(())
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.borrow_mut().assign(name, value),
                None => Err(()),
            }
        }
    }

    pub fn get_enclosing_values(&self) -> Option<HashMap<String, Literal>> {
        match &self.enclosing {
            Some(enclosing) => Some(enclosing.borrow().clone().values),
            None => None,
        }
    }

    pub fn set_values(&mut self, new_values: HashMap<String, Literal>) {
        self.values = new_values;
    }
}
