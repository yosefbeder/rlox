use super::environment::{DataType, Environment};
use super::error::{Error, ErrorReporter};
use super::parser::{Expr, Statement};
use super::scanner::{Token, TokenKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::Instant;

#[derive(Clone)]
pub enum Callable {
    Print,
    Clock,
    User {
        closure: Rc<RefCell<Environment>>,
        parameters: Vec<String>,
        body: Rc<Vec<Statement>>,
    },
}

impl Callable {
    fn call<'b>(
        &self,
        arguments: Vec<DataType>,
        interpreter: &'b Interpreter,
        start_time: Instant,
    ) -> Result<DataType, Error> {
        match self {
            Self::Print => {
                println!("{}", arguments[0].to_string());
                Ok(DataType::Nil)
            }
            Self::Clock => Ok(DataType::Number(start_time.elapsed().as_millis() as f64)),
            Self::User {
                parameters,
                body,
                closure,
            } => {
                let environment = Environment::new(Some(Rc::clone(closure)));
                let zip = parameters.iter().zip(arguments.iter());

                for (parameter, argument) in zip {
                    environment
                        .borrow_mut()
                        .define(parameter, argument.clone())
                        .unwrap();
                }

                for statement in body.iter() {
                    match interpreter.statement(statement, Rc::clone(&environment)) {
                        Ok(value) => match value {
                            Some(value) => return Ok(value),
                            None => continue,
                        },
                        Err(err) => return Err(err),
                    };
                }

                Ok(DataType::Nil)
            }
        }
    }

    fn arty(&self) -> usize {
        match self {
            Self::Print => 1,
            Self::Clock => 0,
            Self::User {
                parameters,
                body: _,
                closure: _,
            } => parameters.len(),
        }
    }
}

impl Expr {
    fn get_line(&self) -> usize {
        match self {
            Expr::Literal(token) => token.line,
            Expr::Unary(operator, _expression) => operator.line,
            Expr::Binary(operator, _expression_1, _expression_2) => operator.line,
            Expr::FnCall(callee, _arguments) => callee.get_line(),
            Expr::Lamda(token, _parameters, _body) => token.line,
        }
    }
}

pub struct Interpreter {
    start_time: Instant,
    globals: Rc<RefCell<Environment>>,
    locals: HashMap<*const Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Environment::new(None);

        globals
            .borrow_mut()
            .define("print", DataType::Fun(Callable::Print))
            .unwrap();
        globals
            .borrow_mut()
            .define("clock", DataType::Fun(Callable::Clock))
            .unwrap();

        Self {
            start_time: Instant::now(),
            locals: HashMap::new(),
            globals,
        }
    }

    pub fn resolve(&mut self, expression_ptr: *const Expr, depth: usize) {
        self.locals.insert(expression_ptr, depth);
    }

    fn statement(
        &self,
        statement: &Statement,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<Option<DataType>, Error> {
        match statement {
            Statement::Print(expr) => {
                println!("{}", self.expression(expr, environment)?.to_string());
                Ok(None)
            }
            Statement::Expr(expr) => {
                self.expression(expr, environment)?;
                Ok(None)
            }
            Statement::VarDecl(token, name, initializer) => {
                let right = match initializer {
                    Some(expr) => self.expression(expr, Rc::clone(&environment))?,
                    None => DataType::Nil,
                };

                match environment.borrow_mut().define(name, right) {
                    Ok(_) => Ok(None),
                    Err(_) => Err(Error::Runtime {
                        message: format!("{} is defined before", name),
                        line: token.line,
                    }),
                }
            }
            Statement::Block(statements) => {
                let local_environment = Environment::new(Some(environment));

                for statement in statements {
                    match self.statement(statement, Rc::clone(&local_environment)) {
                        Ok(value) => match value {
                            Some(value) => return Ok(Some(value)),
                            None => continue,
                        },
                        Err(err) => return Err(err),
                    };
                }

                Ok(None)
            }
            Statement::If(condition, then_branch, else_branch) => {
                if self
                    .expression(condition, Rc::clone(&environment))?
                    .is_truthy()
                {
                    match self.statement(then_branch, Rc::clone(&environment)) {
                        Ok(value) => match value {
                            Some(value) => return Ok(Some(value)),
                            None => {}
                        },
                        Err(err) => return Err(err),
                    };
                } else if let Some(else_branch) = else_branch {
                    match self.statement(else_branch, Rc::clone(&environment)) {
                        Ok(value) => match value {
                            Some(value) => return Ok(Some(value)),
                            None => {}
                        },
                        Err(err) => return Err(err),
                    };
                }
                Ok(None)
            }
            Statement::While(condition, body) => {
                while self
                    .expression(condition, Rc::clone(&environment))?
                    .is_truthy()
                {
                    match self.statement(body, Rc::clone(&environment)) {
                        Ok(value) => match value {
                            Some(value) => return Ok(Some(value)),
                            None => continue,
                        },
                        Err(err) => return Err(err),
                    }
                }
                Ok(None)
            }
            Statement::For(initializer, condition, increment, body) => {
                let local_environment = Environment::new(Some(Rc::clone(&environment)));

                match initializer {
                    Some(statement) => match &**statement {
                        Statement::VarDecl(_line, _name, _expr) => {
                            self.statement(statement, Rc::clone(&local_environment))?;
                        }
                        Statement::Expr(expr) => {
                            self.expression(expr, Rc::clone(&environment))?;
                        }
                        _ => {}
                    },
                    _ => {}
                };

                match condition {
                    Some(expr) => {
                        while self
                            .expression(expr, Rc::clone(&local_environment))?
                            .is_truthy()
                        {
                            match self.statement(body, Rc::clone(&local_environment)) {
                                Ok(value) => match value {
                                    Some(value) => return Ok(Some(value)),
                                    None => {}
                                },
                                Err(err) => return Err(err),
                            };
                            match increment {
                                Some(expr) => {
                                    self.expression(expr, Rc::clone(&local_environment))?;
                                }
                                None => {}
                            }
                        }
                    }
                    None => {}
                }

                Ok(None)
            }
            Statement::Fun(token, name, parameters, body) => {
                match environment.borrow_mut().define(
                    name,
                    DataType::Fun(Callable::User {
                        parameters: parameters.clone(),
                        body: Rc::clone(body),
                        closure: Rc::clone(&environment),
                    }),
                ) {
                    Ok(_) => Ok(None),
                    Err(_) => Err(Error::Runtime {
                        message: format!("{} is already defined", name),
                        line: token.line,
                    }),
                }
            }
            Statement::Return(_token, expression) => match expression {
                Some(expression) => Ok(Some(self.expression(expression, Rc::clone(&environment))?)),
                None => Ok(Some(DataType::Nil)),
            },
        }
    }

    fn expression(
        &self,
        expression: &Expr,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Error> {
        match expression {
            Expr::Literal(token) => Ok(match &token.kind {
                TokenKind::Identifier(name) => {
                    let depth = self.locals.get(&(expression as *const Expr));

                    match depth {
                        Some(depth) => environment.borrow().get_at(name, *depth).unwrap(),
                        None => match self.globals.borrow().get(name) {
                            Some(value) => value,
                            None => {
                                return Err(Error::Runtime {
                                    message: format!("{} is undefined", name),
                                    line: token.line,
                                });
                            }
                        },
                    }
                }
                value => DataType::try_from(value.clone()).unwrap(),
            }),
            Expr::Unary(operator, expression) => {
                let right = self.expression(expression, environment)?;

                match operator.kind {
                    TokenKind::Bang => {
                        if right.is_truthy() {
                            Ok(DataType::False)
                        } else {
                            Ok(DataType::True)
                        }
                    }
                    TokenKind::Minus => match right {
                        DataType::Number(value) => Ok(DataType::Number(value * -1.0)),
                        _ => Err(Error::Runtime {
                            message: String::from("The negative operator works only with numbers"),
                            line: operator.line,
                        }),
                    },
                    _ => Ok(DataType::Nil),
                }
            }
            Expr::Binary(operator, expression_1, expression_2) => {
                let left = self.expression(expression_1, Rc::clone(&environment))?;
                match operator.kind {
                    TokenKind::And => {
                        if !left.is_truthy() {
                            return Ok(left);
                        }

                        return Ok(self.expression(expression_2, Rc::clone(&environment))?);
                    }
                    TokenKind::Or => {
                        if left.is_truthy() {
                            return Ok(left);
                        }

                        return Ok(self.expression(expression_2, Rc::clone(&environment))?);
                    }
                    _ => {}
                }

                let right = self.expression(expression_2, Rc::clone(&environment))?;

                match operator.kind {
                    TokenKind::Plus => match (left, right) {
                        (DataType::String(left), DataType::String(right)) => {
                            Ok(DataType::String(format!("{}{}", left, right)))
                        }
                        (DataType::Number(left), DataType::Number(right)) => {
                            Ok(DataType::Number(left + right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from(
                                "Both operands should be either strings or numbers",
                            ),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Minus => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => {
                            Ok(DataType::Number(left - right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Star => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => {
                            Ok(DataType::Number(left * right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Slash => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => {
                            Ok(DataType::Number(left / right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::BangEqual => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => Ok(if left != right {
                            DataType::True
                        } else {
                            DataType::False
                        }),
                        (DataType::String(left), DataType::String(right)) => Ok(if left != right {
                            DataType::True
                        } else {
                            DataType::False
                        }),
                        (DataType::Nil, DataType::Nil) => Ok(DataType::False),
                        (DataType::False, DataType::False) => Ok(DataType::False),
                        (DataType::True, DataType::True) => Ok(DataType::False),
                        _ => Ok(DataType::True),
                    },
                    TokenKind::EqualEqual => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => Ok(if left == right {
                            DataType::True
                        } else {
                            DataType::False
                        }),
                        (DataType::String(left), DataType::String(right)) => Ok(if left == right {
                            DataType::True
                        } else {
                            DataType::False
                        }),
                        (DataType::Nil, DataType::Nil) => Ok(DataType::True),
                        (DataType::False, DataType::False) => Ok(DataType::True),
                        (DataType::True, DataType::True) => Ok(DataType::True),
                        _ => Ok(DataType::False),
                    },
                    TokenKind::Greater => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => Ok(if left > right {
                            DataType::True
                        } else {
                            DataType::False
                        }),
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::GreaterEqual => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => Ok(if left >= right {
                            DataType::True
                        } else {
                            DataType::False
                        }),
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Less => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => Ok(if left < right {
                            DataType::True
                        } else {
                            DataType::False
                        }),
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::LessEqual => match (left, right) {
                        (DataType::Number(left), DataType::Number(right)) => Ok(if left <= right {
                            DataType::True
                        } else {
                            DataType::False
                        }),
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Equal => match &**expression_1 {
                        Expr::Literal(Token {
                            kind: TokenKind::Identifier(name),
                            line,
                        }) => {
                            let depth = self.locals.get(&(&**expression_1 as *const Expr));

                            match depth {
                                Some(depth) => {
                                    environment
                                        .borrow_mut()
                                        .assign_at(name, right.clone(), *depth)
                                        .unwrap();
                                    Ok(right)
                                }
                                None => {
                                    match self.globals.borrow_mut().assign(name, right.clone()) {
                                        Ok(_) => Ok(right),
                                        Err(_) => Err(Error::Runtime {
                                            message: format!("{} is undefined", name),
                                            line: *line,
                                        }),
                                    }
                                }
                            }
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Bad assignment target"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Comma => Ok(right),
                    _ => Ok(DataType::Nil),
                }
            }
            Expr::FnCall(callee, arguments) => {
                let interpreted_callee = self.expression(callee, Rc::clone(&environment))?;
                let mut interpreted_arguments = vec![];

                for result in arguments
                    .iter()
                    .map(|argument| self.expression(argument, Rc::clone(&environment)))
                {
                    match result {
                        Ok(argument) => interpreted_arguments.push(argument),
                        Err(err) => return Err(err),
                    }
                }

                match interpreted_callee {
                    DataType::Fun(callable) => {
                        let arty = callable.arty();
                        let arguments_count = interpreted_arguments.len();

                        if arty == arguments_count {
                            return callable.call(interpreted_arguments, self, self.start_time);
                        } else {
                            return Err(Error::Runtime {
                                message: format!(
                                    "Expected {} argument{} but got {} argument{}",
                                    arty,
                                    if arty != 1 { "s" } else { "" },
                                    arguments_count,
                                    if arguments_count != 1 { "s" } else { "" }
                                ),
                                line: callee.get_line(),
                            });
                        }
                    }
                    _ => {
                        return Err(Error::Runtime {
                            message: String::from("Callee didn't evaluate to a function"),
                            line: callee.get_line(),
                        });
                    }
                }
            }
            Expr::Lamda(_token, parameters, body) => Ok(DataType::Fun(Callable::User {
                parameters: parameters.clone(),
                body: Rc::clone(body),
                closure: Rc::clone(&environment),
            })),
        }
    }

    pub fn interpret<T: ErrorReporter>(&mut self, ast: &[Statement], error_reporter: &mut T) {
        for statement in ast {
            match self.statement(statement, Rc::clone(&self.globals)) {
                Ok(_) => {}
                Err(error) => {
                    error_reporter.report(error);
                    return;
                }
            };
        }
    }
}
