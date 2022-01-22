pub mod environment;
pub mod interpreter;
pub mod parser;
pub mod scanner;
use std::fmt;

#[derive(Debug)]
pub enum Error {
    Syntax { message: String, line: usize },
    Runtime { message: String, line: usize },
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Syntax { message, line } => {
                write!(f, "[SyntaxError]: {} [line: {}]", message, line)
            }
            Self::Runtime { message, line } => {
                write!(f, "[RuntimeError]: {} [line: {}]", message, line)
            }
        }
    }
}
