use super::interpreter::Callable;
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
    Fun(Callable),
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
            Self::Fun(callable) => match callable {
                Callable::Print => String::from("<native fn>"),
                Callable::Clock => String::from("<native fn>"),
                Callable::User {
                    parameters: _,
                    body: _,
                } => format!("<user fn>"),
            },
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

    pub fn init_globals(&mut self) {
        self.define("print", DataType::Fun(Callable::Print))
            .unwrap();
        self.define("clock", DataType::Fun(Callable::Clock))
            .unwrap();
    }

    pub fn get(&self, name: &str) -> Option<DataType> {
        match self {
            Self::Cons(values, enclosing) => match values.get(name) {
                Some(value) => Some(value.clone()),
                None => enclosing.borrow().get(name),
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

    pub fn assign(&mut self, name: &str, value: DataType) -> Result<(), ()> {
        match self {
            Self::Cons(values, enclosing) => {
                if values.contains_key(name) {
                    values.insert(name.to_string(), value);
                    Ok(())
                } else {
                    enclosing.borrow_mut().assign(name, value)
                }
            }
            Self::Nil => Err(()),
        }
    }
}
