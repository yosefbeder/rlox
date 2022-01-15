use rlox::parser;
use rlox::scanner;
use std::env;
use std::fs;
use std::str;

fn main() {
    let mut args = env::args();
    args.next();

    if let Some(path) = args.next() {
        match fs::read(path) {
            Ok(buffer) => {
                let code = str::from_utf8(&buffer).unwrap();
                run(code);
            }
            Err(_) => println!("Couldn't read the passed file"),
        };
    } else {
        println!("Please enter the file path")
    }
}

fn run(code: &str) {
    match scanner::scan(&code) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(statements) => {
                for statement in statements {
                    match statement.interpret() {
                        Ok(_) => (),
                        Err(err) => println!("{}", err),
                    };
                }
            }
            Err(errs) => {
                for err in errs {
                    println!("{}", err)
                }
            }
        },
        Err(err) => println!("{}", err),
    }
}
