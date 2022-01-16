use std::fmt;

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
        write!(f, "[SyntaxError]: {} [line: {}]", self.message, self.line)
    }
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    message: String,
    line: usize,
}

impl RuntimeError {
    pub fn new(message: String, line: usize) -> Self {
        Self { message, line }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[RuntimeError]: {} [line: {}]", self.message, self.line)
    }
}
