use super::parser::Literal;
use std::collections::HashMap;

pub struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new(enclosing: Option<Self>) -> Self {
        Self {
            enclosing: match enclosing {
                Some(environment) => Some(Box::new(environment)),
                None => None,
            },
            values: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Literal> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.enclosing {
                Some(enclosing) => match enclosing.values.get(name) {
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

    pub fn assign(&mut self, name: &str, value: Literal) -> Result<(), ()> {
        match self.values.insert(String::from(name), value) {
            Some(_) => Ok(()),
            None => Err(()),
        }
    }
}
