use super::scanner::TokenKind;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub trait CallableClone {
    fn clone_box(&self) -> Box<dyn Callable>;
}

pub trait Callable: CallableClone {
    fn arty(&self) -> usize;
    fn call(&self, arguments: Vec<DataType>);
}

impl<T> CallableClone for T
where
    T: 'static + Callable + Clone,
{
    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Callable> {
    fn clone(&self) -> Box<dyn Callable> {
        self.clone_box()
    }
}

//TODO remove clonning and instead give a reference
#[derive(Clone)]
pub enum DataType {
    Identifier(String),
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Fun(Box<dyn Callable>),
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
            _ => String::new(),
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

#[derive(Clone)]
pub struct PrintFun {}

impl Callable for PrintFun {
    fn arty(&self) -> usize {
        1
    }
    fn call(&self, arguments: Vec<DataType>) {
        for argument in arguments {
            println!("{}", argument.to_string())
        }
    }
}

impl PrintFun {
    pub fn new() -> DataType {
        DataType::Fun(Box::new(PrintFun {}))
    }
}

pub enum Environment {
    Cons(HashMap<String, DataType>, Rc<RefCell<Environment>>),
    Nil,
}

impl Environment {
    pub fn new(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self::Cons(HashMap::new(), enclosing)
    }

    pub fn init_globals(&mut self) {
        self.define("print", &PrintFun::new()).unwrap();
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

    pub fn define(&mut self, name: &str, value: &DataType) -> Result<(), ()> {
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

    pub fn assign(&mut self, name: &str, value: &DataType) -> Result<(), ()> {
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
