use super::parser::Literal;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub enum Environment {
    Cons(HashMap<String, Literal>, Rc<RefCell<Environment>>),
    Nil,
}

impl Environment {
    pub fn new(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self::Cons(HashMap::new(), enclosing)
    }

    pub fn get(&self, name: &str) -> Option<Literal> {
        match self {
            Self::Cons(values, enclosing) => match values.get(name) {
                Some(value) => Some(value.clone()),
                None => enclosing.borrow().get(name),
            },
            Self::Nil => None,
        }
    }

    pub fn define(&mut self, name: &str, value: &Literal) -> Result<(), ()> {
        match self {
            Self::Cons(values, _enclosing) => {
                match values.insert(name.to_string(), value.clone()) {
                    Some(_) => Err(()),
                    None => Ok(()),
                }
            }
            Self::Nil => Err(()),
        }
    }

    pub fn assign(&mut self, name: &str, value: &Literal) -> Result<(), ()> {
        match self {
            Self::Cons(values, enclosing) => {
                if values.contains_key(name) {
                    values.insert(name.to_string(), value.clone());
                    Ok(())
                } else {
                    enclosing.borrow_mut().assign(name, value)
                }
            }
            Self::Nil => Err(()),
        }
    }
}
