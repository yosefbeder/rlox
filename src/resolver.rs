use super::error::{Error, ErrorReporter};
use super::interpreter::Interpreter;
use super::parser::{Expr, Statement};
use super::scanner::TokenKind;
use std::collections::HashMap;

pub struct Resolver<'a> {
    program: &'a [Statement],
    scopes: Vec<HashMap<String, bool>>,
}

impl<'a> Resolver<'a> {
    pub fn new(program: &'a [Statement]) -> Self {
        Self {
            program,
            scopes: vec![],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) {
        if self.scopes.len() == 0 {
            return;
        }

        self.scopes
            .last_mut()
            .unwrap()
            .insert(String::from(name), false);
    }

    fn define(&mut self, name: &str) {
        if self.scopes.len() == 0 {
            return;
        }
        self.scopes
            .last_mut()
            .unwrap()
            .insert(String::from(name), true);
    }

    fn resolve_local(&mut self, name: &str, expression: &Expr, interpreter: &mut Interpreter) {
        let mut depth = 0;
        let mut scopes_iter = self.scopes.iter();
        while let Some(scope) = scopes_iter.next_back() {
            if scope.contains_key(name) {
                interpreter.resolve(expression as *const Expr, depth);
                break;
            }
            depth = depth + 1;
        }
    }

    fn expression(
        &mut self,
        expression: &Expr,
        interpreter: &mut Interpreter,
    ) -> Result<(), Error> {
        match expression {
            Expr::Literal(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    if self.scopes.len() != 0
                        && self.scopes.last().unwrap().get(name).is_some()
                        && !self.scopes.last().unwrap().get(name).unwrap()
                    {
                        return Err(Error::Static {
                            message: String::from(
                                "Can't reference a variable inside it's declaration",
                            ),
                            line: token.line,
                        });
                    }

                    self.resolve_local(name, expression, interpreter);
                    Ok(())
                }
                _ => Ok(()),
            },
            Expr::Binary(_token, expression_1, expression_2) => {
                self.expression(expression_1, interpreter)?;
                self.expression(expression_2, interpreter)?;
                Ok(())
            }
            Expr::FnCall(expression, arguments) => {
                self.expression(expression, interpreter)?;
                for argument in arguments {
                    self.expression(argument, interpreter)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn statement(
        &mut self,
        statement: &Statement,
        interpreter: &mut Interpreter,
    ) -> Result<(), Error> {
        match statement {
            Statement::Block(statements) => {
                self.push_scope();
                for statement in statements {
                    self.statement(statement, interpreter)?;
                }
                self.pop_scope();
                Ok(())
            }
            Statement::VarDecl(_token, name, initializer) => {
                self.declare(name);
                match initializer {
                    Some(expression) => self.expression(expression, interpreter)?,
                    None => {}
                }
                self.define(name);
                Ok(())
            }
            Statement::Fun(_token, name, parameters, body) => {
                self.declare(name);
                self.define(name);
                self.push_scope();

                for parameter in parameters {
                    self.declare(parameter);
                    self.define(parameter);
                }

                for statement in body.iter() {
                    self.statement(statement, interpreter)?;
                }

                self.pop_scope();

                Ok(())
            }
            Statement::Expr(expression) => {
                self.expression(expression, interpreter)?;

                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub fn resolve<T: ErrorReporter>(
        &mut self,
        interpreter: &mut Interpreter,
        error_reporter: &mut T,
    ) {
        for statement in self.program {
            match self.statement(statement, interpreter) {
                Ok(_) => {
                    continue;
                }
                Err(error) => {
                    error_reporter.report(error);
                    break;
                }
            };
        }
    }
}
