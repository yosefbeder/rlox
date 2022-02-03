use rlox::error::{Error, ErrorReporter};
use rlox::interpreter::Interpreter;
use rlox::parser::Parser;
use rlox::resolver::Resolver;
use rlox::scanner::Scanner;
use std::env;
use std::fs;
use std::io;
use std::process;

struct REPLErrorReporter {
    errors_count: usize,
}

impl REPLErrorReporter {
    fn new() -> Self {
        Self { errors_count: 0 }
    }

    fn reset(&mut self) {
        self.errors_count = 0;
    }
}

impl ErrorReporter for REPLErrorReporter {
    fn report(&mut self, error: Error) {
        self.errors_count += 1;
        match error {
            Error::Syntax { message, line: _ } => {
                eprintln!("[SyntaxError]: {}", message)
            }
            Error::Static { message, line: _ } => {
                eprintln!("[StaticError]: {}", message)
            }
            Error::Runtime { message, line: _ } => {
                eprintln!("[RuntimeError]: {}", message)
            }
        }
    }

    fn has_error(&self) -> bool {
        self.errors_count > 0
    }
}

struct FileErrorReporter {
    errors_count: usize,
}

impl FileErrorReporter {
    fn new() -> Self {
        Self { errors_count: 0 }
    }
}

impl ErrorReporter for FileErrorReporter {
    fn report(&mut self, error: Error) {
        self.errors_count += 1;
        eprintln!("{}", error);
    }

    fn has_error(&self) -> bool {
        self.errors_count > 0
    }
}

fn main() {
    let mut args = env::args();
    args.next();
    if let Some(path) = args.next() {
        run_file(path);
    } else {
        run_repl();
    }
}

fn run<T: ErrorReporter>(code: &str, interpreter: &mut Interpreter, error_reporter: &mut T) {
    let tokens = Scanner::new(&code, error_reporter).scan();
    if error_reporter.has_error() {
        return;
    }

    let ast = Parser::new(&tokens, error_reporter).parse();
    if error_reporter.has_error() {
        return;
    }

    let mut resolver = Resolver::new(&ast);
    resolver.resolve(interpreter, error_reporter);
    if error_reporter.has_error() {
        return;
    }

    interpreter.interpret(&ast, error_reporter);
}

fn run_repl() {
    let mut interpreter = Interpreter::new();
    let mut error_reporter = REPLErrorReporter::new();

    loop {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(_) => {
                if line.trim().len() == 0 {
                    break;
                }
                run(&line, &mut interpreter, &mut error_reporter);
                error_reporter.reset();
            }
            Err(err) => {
                eprintln!("{:?}", err);
                process::exit(71);
            }
        }
    }
}

fn run_file(path: String) {
    let mut interpreter = Interpreter::new();
    let code = match fs::read_to_string(path) {
        Ok(value) => value,
        Err(err) => {
            eprintln!("{}", err);
            process::exit(65);
        }
    };
    let mut error_reporter = FileErrorReporter::new();

    run(&code, &mut interpreter, &mut error_reporter);
}
