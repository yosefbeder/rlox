use super::environment::{DataType, Environment};
use super::error::{Error, ErrorReporter};
use super::parser::{Expr, Statement};
use super::scanner::{Token, TokenKind};
use std::cell::RefCell;
use std::rc::Rc;

impl Expr {
    fn get_line(&self) -> usize {
        match self {
            Expr::Literal(token) => token.line,
            Expr::Unary(operator, _expression) => operator.line,
            Expr::Binary(operator, _expression_1, _expression_2) => operator.line,
            Expr::FnCall(callee, _arguments) => callee.get_line(),
        }
    }
}

pub struct Interpreter<'a, 'b, T: ErrorReporter> {
    program: &'a [Statement],
    error_reporter: &'b mut T,
}

impl<'a, 'b, T: ErrorReporter> Interpreter<'a, 'b, T> {
    pub fn new(program: &'a [Statement], error_reporter: &'b mut T) -> Self {
        Self {
            program,
            error_reporter,
        }
    }

    fn statement(
        &self,
        statement: &Statement,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), Error> {
        match statement {
            Statement::Print(expr) => {
                println!("{}", self.expression(expr, environment)?.to_string());
                Ok(())
            }
            Statement::Expr(expr) => {
                self.expression(expr, environment)?;
                Ok(())
            }
            Statement::VarDecl(line, name, initializer) => {
                let right = match initializer {
                    Some(expr) => self.expression(expr, Rc::clone(&environment))?,
                    None => DataType::Nil,
                };

                match environment.borrow_mut().define(name, &right) {
                    Ok(_) => Ok(()),
                    Err(_) => Err(Error::Runtime {
                        message: format!("{} is defined before", name),
                        line: *line,
                    }),
                }
            }
            Statement::Block(statements) => {
                let local_environment = Rc::new(RefCell::new(Environment::new(environment)));

                for statement in statements {
                    self.statement(statement, Rc::clone(&local_environment))?;
                }

                Ok(())
            }
            Statement::If(condition, then_branch, else_branch) => {
                if self
                    .expression(condition, Rc::clone(&environment))?
                    .is_truthy()
                {
                    self.statement(then_branch, Rc::clone(&environment))?;
                } else if let Some(else_branch) = else_branch {
                    self.statement(else_branch, Rc::clone(&environment))?;
                }
                Ok(())
            }
            Statement::While(condition, body) => {
                while self
                    .expression(condition, Rc::clone(&environment))?
                    .is_truthy()
                {
                    self.statement(body, Rc::clone(&environment))?;
                }
                Ok(())
            }
            Statement::For(initializer, condition, increment, body) => {
                let local_environment =
                    Rc::new(RefCell::new(Environment::new(Rc::clone(&environment))));

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
                            self.statement(body, Rc::clone(&local_environment))?;
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

                Ok(())
            }
        }
    }

    fn expression(
        &self,
        expression: &Expr,
        environment: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Error> {
        match expression {
            Expr::Literal(token) => Ok(match &token.kind {
                TokenKind::Identifier(name) => match environment.borrow().get(&name) {
                    Some(value) => value,
                    None => {
                        return Err(Error::Runtime {
                            message: format!("{} is undefined", name),
                            line: token.line,
                        });
                    }
                },
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
                        }) => match environment.borrow_mut().assign(name, &right) {
                            Ok(()) => Ok(right),
                            Err(_) => Err(Error::Runtime {
                                message: format!("{} is undefined", name),
                                line: *line,
                            }),
                        },
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
                            callable.call(interpreted_arguments);
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

                Ok(DataType::Nil)
            }
        }
    }

    pub fn interpret(&mut self, environment: Rc<RefCell<Environment>>) {
        for statement in self.program.iter() {
            match self.statement(statement, Rc::clone(&environment)) {
                Ok(()) => {}
                Err(error) => {
                    self.error_reporter.report(error);
                    return;
                }
            };
        }
    }
}
