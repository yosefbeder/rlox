use super::environment::Environment;
use super::parser::{BinaryOperator, Expr, Literal, Statement, UnaryOperator};
use super::Error;

pub struct Interpreter {
    program: Vec<Statement>,
}

impl Interpreter {
    pub fn new(program: Vec<Statement>) -> Self {
        Self { program }
    }

    fn statement(&self, statement: &Statement, environment: &mut Environment) -> Result<(), Error> {
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
                    Some(expr) => self.expression(expr, environment)?,
                    None => Literal::Nil,
                };

                match environment.define(name, right) {
                    Ok(_) => Ok(()),
                    Err(_) => Err(Error::Runtime {
                        message: format!("{} is defined before", name),
                        line: *line,
                    }),
                }
            }
            Statement::Block(declarations) => {
                for declaration in declarations {
                    println!("{:?}", declaration);
                }
                Ok(())
            }
        }
    }

    fn expression(
        &self,
        expression: &Expr,
        environment: &mut Environment,
    ) -> Result<Literal, Error> {
        match expression {
            Expr::Literal(line, literal) => Ok(match literal {
                Literal::Identifier(name) => match environment.get(name) {
                    Some(value) => value.clone(),
                    None => {
                        return Err(Error::Runtime {
                            message: format!("{} is undefined", name),
                            line: *line,
                        });
                    }
                },
                value => value.clone(),
            }),
            Expr::Unary(line, operator, expr) => {
                let right = self.expression(expr, environment)?;

                match operator {
                    UnaryOperator::Bang => {
                        if right.is_truthy() {
                            Ok(Literal::False)
                        } else {
                            Ok(Literal::True)
                        }
                    }
                    UnaryOperator::Minus => match right {
                        Literal::Number(value) => Ok(Literal::Number(value * -1.0)),
                        _ => Err(Error::Runtime {
                            message: String::from("The negative operator works only with numbers"),
                            line: *line,
                        }),
                    },
                }
            }
            Expr::Binary(line, operator, expr_1, expr_2) => {
                let left = self.expression(expr_1, environment)?;
                let right = self.expression(expr_2, environment)?;

                match operator {
                    BinaryOperator::Plus => match (left, right) {
                        (Literal::String(left), Literal::String(right)) => {
                            Ok(Literal::String(format!("{}{}", left, right)))
                        }
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left + right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from(
                                "Both operands should be either strings or numbers",
                            ),
                            line: *line,
                        }),
                    },
                    BinaryOperator::Minus => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left - right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: *line,
                        }),
                    },
                    BinaryOperator::Star => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left * right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: *line,
                        }),
                    },
                    BinaryOperator::Slash => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left / right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: *line,
                        }),
                    },
                    BinaryOperator::BangEqual => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left != right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        (Literal::String(left), Literal::String(right)) => Ok(if left != right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        (Literal::Nil, Literal::Nil) => Ok(Literal::False),
                        (Literal::False, Literal::False) => Ok(Literal::False),
                        (Literal::True, Literal::True) => Ok(Literal::False),
                        _ => Ok(Literal::True),
                    },
                    BinaryOperator::EqualEqual => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left == right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        (Literal::String(left), Literal::String(right)) => Ok(if left == right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        (Literal::Nil, Literal::Nil) => Ok(Literal::True),
                        (Literal::False, Literal::False) => Ok(Literal::True),
                        (Literal::True, Literal::True) => Ok(Literal::True),
                        _ => Ok(Literal::False),
                    },
                    BinaryOperator::Greater => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left > right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: *line,
                        }),
                    },
                    BinaryOperator::GreaterEqual => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left >= right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: *line,
                        }),
                    },
                    BinaryOperator::Less => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left < right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: *line,
                        }),
                    },
                    BinaryOperator::LessEqual => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left <= right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        _ => Err(Error::Runtime {
                            message: String::from("Both operands should be numbers"),
                            line: *line,
                        }),
                    },
                    BinaryOperator::Comma => Ok(right),
                    BinaryOperator::Equal => match &**expr_1 {
                        Expr::Literal(line, Literal::Identifier(name)) => {
                            match environment.assign(name, right.clone()) {
                                Ok(()) => Ok(right),
                                Err(_) => Err(Error::Runtime {
                                    message: format!("{} is undefined", name),
                                    line: *line,
                                }),
                            }
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Bad assignment target"),
                            line: expr_1.get_line(),
                        }),
                    },
                }
            }
        }
    }

    pub fn interpret(&self, environment: &mut Environment) -> Result<(), Error> {
        for statement in self.program.iter() {
            match self.statement(statement, environment) {
                Ok(()) => {}
                Err(err) => return Err(err),
            };
        }
        Ok(())
    }
}
