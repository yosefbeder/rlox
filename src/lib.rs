use std::fmt;
mod parser;
mod scanner;

#[derive(Debug, PartialEq)]
pub struct SyntaxError {
    message: String,
    line: usize,
}

impl SyntaxError {
    pub fn new(message: String, line: usize) -> Self {
        Self { message, line }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at line number {}", self.message, self.line)
    }
}
