use rlox::environment::Environment;
use rlox::interpreter::Interpreter;
use rlox::parser::Parser;
use rlox::scanner::Scanner;
use rlox::Error;
use std::env;
use std::fs;
use std::io;
use std::process;

fn main() {
    let mut args = env::args();
    args.next();
    if let Some(path) = args.next() {
        run_file(path);
    } else {
        run_repl();
    }
}

fn run(code: String, environment: &mut Environment) -> Result<(), Vec<Error>> {
    let tokens = match Scanner::new(code).scan() {
        Ok(value) => value,
        Err(err) => {
            return Err(vec![err]);
        }
    };

    let ast = match Parser::new(tokens).parse() {
        Ok(value) => value,
        Err(errs) => {
            return Err(errs);
        }
    };

    match Interpreter::new(ast).interpret(environment) {
        Ok(()) => {}
        Err(err) => {
            return Err(vec![err]);
        }
    };

    Ok(())
}

fn run_repl() {
    let mut environment = Environment::new(None);
    loop {
        let mut line = String::new();

        match io::stdin().read_line(&mut line) {
            Ok(_) => {
                if line.trim().len() == 0 {
                    break;
                }

                match run(line, &mut environment) {
                    Ok(_) => {}
                    Err(errs) => eprintln!("{}", errs[0]),
                }
            }
            Err(err) => {
                eprintln!("{:?}", err);
                process::exit(71);
            }
        }
    }
}

fn run_file(path: String) {
    let code = match fs::read_to_string(path) {
        Ok(value) => value,
        Err(err) => {
            eprintln!("{}", err);
            process::exit(65);
        }
    };
    let mut environment = Environment::new(None);

    match run(code, &mut environment) {
        Ok(_) => {}
        Err(errs) => {
            for err in errs {
                eprintln!("{}", err);
            }
            process::exit(65);
        }
    };
}
