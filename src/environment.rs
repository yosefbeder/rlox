use super::interpreter::{Class, Fun, Instance};
use super::scanner::TokenKind;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum DataType {
    Identifier(String),
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Fun(Fun),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
}

impl DataType {
    pub fn to_string(&self) -> String {
        match self {
            Self::String(value) => format!("{}", value),
            Self::Number(value) => format!("{}", value),
            Self::Identifier(value) => format!("{}", value),
            Self::True => String::from("true"),
            Self::False => String::from("false"),
            Self::Nil => String::from("nil"),
            Self::Fun(fun) => match fun {
                Fun::Print => String::from("<native fun>"),
                Fun::Clock => String::from("<native fun>"),
                Fun::User {
                    parameters: _,
                    body: _,
                    closure: _,
                } => format!("<user fun>"),
            },
            Self::Class(class) => format!("<{} class>", class.name),
            Self::Instance(instance) => {
                format!("<{} instance>", instance.borrow().class.name)
            }
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::False => false,
            Self::Nil => false,
            _ => true,
        }
    }
}

impl TryFrom<TokenKind> for DataType {
    type Error = ();
    fn try_from(token_kind: TokenKind) -> Result<Self, Self::Error> {
        match token_kind {
            TokenKind::Identifier(value) => Ok(Self::Identifier(value)),
            TokenKind::String(value) => Ok(Self::String(value)),
            TokenKind::Number(value) => Ok(Self::Number(value)),
            TokenKind::True => Ok(Self::True),
            TokenKind::False => Ok(Self::False),
            TokenKind::Nil => Ok(Self::Nil),
            _ => Err(()),
        }
    }
}

pub enum Environment {
    Cons(HashMap<String, DataType>, Rc<RefCell<Environment>>),
    Nil,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::Cons(
            HashMap::new(),
            match enclosing {
                Some(enclosing) => enclosing,
                None => Rc::new(RefCell::new(Self::Nil)),
            },
        )))
    }

    pub fn get_at(&self, name: &str, depth: usize) -> Option<DataType> {
        if depth == 0 {
            match self {
                Self::Cons(values, _enclosing) => match values.get(name) {
                    Some(value) => Some(value.clone()),
                    None => None,
                },
                Self::Nil => None,
            }
        } else {
            match self {
                Self::Cons(_values, enclosing) => enclosing.borrow().get_at(name, depth - 1),
                Self::Nil => None,
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<DataType> {
        match self {
            Self::Cons(values, _enclosing) => match values.get(name) {
                Some(value) => Some(value.clone()),
                None => None,
            },
            Self::Nil => None,
        }
    }

    pub fn define(&mut self, name: &str, value: DataType) -> Result<(), ()> {
        match self {
            Self::Cons(values, _enclosing) => match values.insert(name.to_string(), value) {
                Some(_) => Err(()),
                None => Ok(()),
            },
            Self::Nil => Err(()),
        }
    }

    pub fn assign_at(&mut self, name: &str, value: DataType, depth: usize) -> Result<(), ()> {
        if depth == 0 {
            match self {
                Self::Cons(values, _enclosing) => {
                    if values.contains_key(name) {
                        self.assign(name, value)?;
                        Ok(())
                    } else {
                        Err(())
                    }
                }
                Self::Nil => Err(()),
            }
        } else {
            match self {
                Self::Cons(_values, enclosing) => {
                    enclosing.borrow_mut().assign_at(name, value, depth - 1)
                }
                Self::Nil => Err(()),
            }
        }
    }

    pub fn assign(&mut self, name: &str, value: DataType) -> Result<(), ()> {
        match self {
            Self::Cons(values, _enclosing) => {
                if values.contains_key(name) {
                    values.insert(String::from(name), value);
                    Ok(())
                } else {
                    Err(())
                }
            }
            Self::Nil => Err(()),
        }
    }
}
