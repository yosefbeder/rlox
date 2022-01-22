use super::environment::Environment;
use super::parser::{Expr, Statement};
use super::scanner::{Token, TokenKind};
use super::Error;
use std::cell::RefCell;
use std::rc::Rc;

impl TokenKind {
    pub fn to_string(&self) -> String {
        match self {
            Self::String(value) => format!("\"{}\"", value),
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

pub struct Interpreter {
    program: Vec<Statement>,
}

impl Interpreter {
    pub fn new(program: Vec<Statement>) -> Self {
        Self { program }
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
                    None => TokenKind::Nil,
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
    ) -> Result<TokenKind, Error> {
        match expression {
            Expr::Literal(token) => Ok(match &token.kind {
                TokenKind::Identifier(name) => match environment.borrow().get(&name) {
                    Some(value) => value.clone(),
                    None => {
                        return Err(Error::Runtime {
                            message: format!("{} is undefined", name),
                            line: token.line,
                        });
                    }
                },
                value => value.clone(),
            }),
            Expr::Unary(operator, expression) => {
                let right = self.expression(expression, environment)?;

                match operator.kind {
                    TokenKind::Bang => {
                        if right.is_truthy() {
                            Ok(TokenKind::False)
                        } else {
                            Ok(TokenKind::True)
                        }
                    }
                    TokenKind::Minus => match right {
                        TokenKind::Number(value) => Ok(TokenKind::Number(value * -1.0)),
                        _ => Err(Error::Runtime {
                            message: String::from("The negative operator works only with numbers"),
                            line: operator.line,
                        }),
                    },
                    _ => Ok(TokenKind::Nil),
                }
            }
            Expr::Binary(operator, expression_1, expression_2) => {
                match operator.kind {
                    TokenKind::Comma => {
                        return Ok(self.expression(expression_2, Rc::clone(&environment))?);
                    }
                    _ => {}
                }

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
                        (TokenKind::String(left), TokenKind::String(right)) => {
                            Ok(TokenKind::String(format!("{}{}", left, right)))
                        }
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(TokenKind::Number(left + right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from(
                                "Both operands should be either strings or numbers",
                            ),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Minus => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(TokenKind::Number(left - right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Star => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(TokenKind::Number(left * right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Slash => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(TokenKind::Number(left / right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::BangEqual => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(if left != right {
                                TokenKind::True
                            } else {
                                TokenKind::False
                            })
                        }
                        (TokenKind::String(left), TokenKind::String(right)) => {
                            Ok(if left != right {
                                TokenKind::True
                            } else {
                                TokenKind::False
                            })
                        }
                        (TokenKind::Nil, TokenKind::Nil) => Ok(TokenKind::False),
                        (TokenKind::False, TokenKind::False) => Ok(TokenKind::False),
                        (TokenKind::True, TokenKind::True) => Ok(TokenKind::False),
                        _ => Ok(TokenKind::True),
                    },
                    TokenKind::EqualEqual => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(if left == right {
                                TokenKind::True
                            } else {
                                TokenKind::False
                            })
                        }
                        (TokenKind::String(left), TokenKind::String(right)) => {
                            Ok(if left == right {
                                TokenKind::True
                            } else {
                                TokenKind::False
                            })
                        }
                        (TokenKind::Nil, TokenKind::Nil) => Ok(TokenKind::True),
                        (TokenKind::False, TokenKind::False) => Ok(TokenKind::True),
                        (TokenKind::True, TokenKind::True) => Ok(TokenKind::True),
                        _ => Ok(TokenKind::False),
                    },
                    TokenKind::Greater => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(if left > right {
                                TokenKind::True
                            } else {
                                TokenKind::False
                            })
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::GreaterEqual => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(if left >= right {
                                TokenKind::True
                            } else {
                                TokenKind::False
                            })
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::Less => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(if left < right {
                                TokenKind::True
                            } else {
                                TokenKind::False
                            })
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: operator.line,
                        }),
                    },
                    TokenKind::LessEqual => match (left, right) {
                        (TokenKind::Number(left), TokenKind::Number(right)) => {
                            Ok(if left <= right {
                                TokenKind::True
                            } else {
                                TokenKind::False
                            })
                        }
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
                    _ => Ok(TokenKind::Nil),
                }
            }
        }
    }

    pub fn interpret(&self, environment: Rc<RefCell<Environment>>) -> Result<(), Error> {
        for statement in self.program.iter() {
            match self.statement(statement, Rc::clone(&environment)) {
                Ok(()) => {}
                Err(err) => return Err(err),
            };
        }
        Ok(())
    }
}
