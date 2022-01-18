use super::environment::Environment;
use super::errors::RuntimeError;
use super::parser::{BinaryOperator, Expr, Literal, Statement, UnaryOperator};

pub struct Interpreter {
    program: Vec<Statement>,
    environment: Environment,
    current: usize,
}

impl Interpreter {
    pub fn new(program: Vec<Statement>) -> Self {
        Self {
            program,
            environment: Environment::new(None),
            current: 0,
        }
    }

    fn statement(&mut self) -> Result<(), RuntimeError> {
        let statement = self.program.get(self.current).unwrap();

        match statement {
            Statement::Print(expr) => {
                println!(
                    "{}",
                    Self::expression(expr, &mut self.environment)?.to_string()
                );
                self.current += 1;
                Ok(())
            }
            Statement::Expr(expr) => {
                Self::expression(expr, &mut self.environment)?;
                self.current += 1;
                Ok(())
            }
            Statement::VarDecl(line, name, initializer) => {
                let right = match initializer {
                    Some(expr) => Self::expression(expr, &mut self.environment)?,
                    None => Literal::Nil,
                };

                match self.environment.define(name, right) {
                    Ok(_) => {
                        self.current += 1;
                        Ok(())
                    }
                    Err(_) => Err(RuntimeError::new(
                        format!("{} is defined before", name),
                        *line,
                    )),
                }
            }
        }
    }

    fn expression(
        expression: &Expr,
        environment: &mut Environment,
    ) -> Result<Literal, RuntimeError> {
        match expression {
            Expr::Literal(line, literal) => Ok(match literal {
                Literal::Identifier(name) => match environment.get(name) {
                    Some(value) => value.clone(),
                    None => return Err(RuntimeError::new(format!("{} is undefined", name), *line)),
                },
                value => value.clone(),
            }),
            Expr::Unary(line, operator, expr) => {
                let right = Self::expression(expr, environment)?;

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
                        _ => Err(RuntimeError::new(
                            String::from("The negative operator works only with numbers"),
                            *line,
                        )),
                    },
                }
            }
            Expr::Binary(line, operator, expr_1, expr_2) => {
                let left = Self::expression(expr_1, environment)?;
                let right = Self::expression(expr_2, environment)?;

                match operator {
                    BinaryOperator::Plus => match (left, right) {
                        (Literal::String(left), Literal::String(right)) => {
                            Ok(Literal::String(format!("{}{}", left, right)))
                        }
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left + right))
                        }
                        _ => Err(RuntimeError::new(
                            String::from("Both operands should be either strings or numbers"),
                            *line,
                        )),
                    },
                    BinaryOperator::Minus => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left - right))
                        }
                        _ => Err(RuntimeError::new(
                            String::from("Both operands should be numbers"),
                            *line,
                        )),
                    },
                    BinaryOperator::Star => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left * right))
                        }
                        _ => Err(RuntimeError::new(
                            String::from("Both operands should be numbers"),
                            *line,
                        )),
                    },
                    BinaryOperator::Slash => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => {
                            Ok(Literal::Number(left / right))
                        }
                        _ => Err(RuntimeError::new(
                            String::from("Both operands should be numbers"),
                            *line,
                        )),
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
                        _ => Err(RuntimeError::new(
                            String::from("Both operands should be numbers"),
                            *line,
                        )),
                    },
                    BinaryOperator::GreaterEqual => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left >= right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        _ => Err(RuntimeError::new(
                            String::from("Both operands should be numbers"),
                            *line,
                        )),
                    },
                    BinaryOperator::Less => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left < right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        _ => Err(RuntimeError::new(
                            String::from("Both operands should be numbers"),
                            *line,
                        )),
                    },
                    BinaryOperator::LessEqual => match (left, right) {
                        (Literal::Number(left), Literal::Number(right)) => Ok(if left <= right {
                            Literal::True
                        } else {
                            Literal::False
                        }),
                        _ => Err(RuntimeError::new(
                            String::from("Both operands should be numbers"),
                            *line,
                        )),
                    },
                    BinaryOperator::Comma => Ok(right),
                    BinaryOperator::Equal => match &**expr_1 {
                        Expr::Literal(line, Literal::Identifier(name)) => {
                            match environment.assign(name, right.clone()) {
                                Ok(()) => Ok(right),
                                Err(_) => {
                                    Err(RuntimeError::new(format!("{} is undefined", name), *line))
                                }
                            }
                        }
                        _ => Err(RuntimeError::new(
                            String::from("Bad assignment target"),
                            expr_1.get_line(),
                        )),
                    },
                }
            }
        }
    }

    pub fn interpret(&mut self) {
        while let Some(_) = self.program.iter().nth(self.current) {
            match self.statement() {
                Ok(()) => {}
                Err(err) => panic!("{}", err),
            }
        }
    }
}
