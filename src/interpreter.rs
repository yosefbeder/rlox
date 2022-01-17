use super::errors::RuntimeError;
use super::parser::{BinaryOperator, Expr, Literal, Statement, UnaryOperator};
use std::collections::HashMap;

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
        environment: &mut HashMap<String, Literal>,
    ) -> Result<(), RuntimeError> {
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
                if !environment.contains_key(name) {
                    match initializer {
                        Some(expr) => {
                            let value = self.expression(expr, environment)?;
                            environment.insert(name.clone(), value)
                        }
                        None => environment.insert(name.clone(), Literal::Nil),
                    };
                } else {
                    return Err(RuntimeError::new(
                        format!("Couldn't redefine {}", name),
                        *line,
                    ));
                }
                Ok(())
            }
        }
    }

    fn expression(
        &self,
        expression: &Expr,
        environment: &mut HashMap<String, Literal>,
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
                        _ => Err(RuntimeError::new(
                            String::from("The negative operator works only with numbers"),
                            *line,
                        )),
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
                    BinaryOperator::Equal => {
                        match &**expr_1 {
                            Expr::Literal(line, Literal::Identifier(name)) => {
                                if environment.contains_key(name) {
                                    environment.insert(name.clone(), right.clone());
                                    Ok(right)
                                } else {
                                    Err(RuntimeError::new(format!("{} is undefined", name), *line))
                                }
                            }
                            //TODO solve later
                            _ => Err(RuntimeError::new(
                                String::from("Bad assignment target"),
                                expr_1.get_line(),
                            )),
                        }
                    }
                }
            }
        }
    }

    pub fn interpret(&self) {
        let mut environment = HashMap::new();

        for statement in &self.program {
            match self.statement(statement, &mut environment) {
                Ok(_) => (),
                Err(err) => {
                    panic!("{}", err);
                }
            };
        }
    }
}
