use super::error::{Error, ErrorReporter};
use super::interpreter::Interpreter;
use super::parser::{Expr, Statement};
use super::scanner::{Token, TokenKind};
use std::collections::HashMap;

struct Var {
    defined: bool,
    referenced: bool,
    line: usize,
}

impl Var {
    fn new(line: usize) -> Self {
        Self {
            defined: false,
            referenced: false,
            line,
        }
    }

    fn define(&mut self) {
        self.defined = true;
    }

    fn reference(&mut self) {
        self.referenced = true;
    }
}

#[derive(PartialEq)]
enum ScopeKind {
    Block,
    Fun,
}

struct Scope {
    kind: ScopeKind,
    values: HashMap<String, Var>,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            values: HashMap::new(),
        }
    }
}

pub struct Resolver<'a> {
    program: &'a [Statement],
    scopes: Vec<Scope>,
    in_fun: bool,
}

impl<'a> Resolver<'a> {
    pub fn new(program: &'a [Statement]) -> Self {
        Self {
            program,
            scopes: vec![],
            in_fun: false,
        }
    }

    fn push_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind))
    }
    fn pop_scope<T: ErrorReporter>(&mut self, error_reporter: &mut T) {
        let last_scope = self.scopes.pop();

        match last_scope {
            Some(last_scope) => {
                for (key, value) in last_scope.values.iter() {
                    if !value.referenced {
                        error_reporter.report(Error::Static {
                            message: format!("{} is not used", key),
                            line: value.line,
                        });
                    }
                }
            }
            None => {}
        }
    }

    fn declare(&mut self, name: &str, line: usize) {
        if self.scopes.len() == 0 {
            return;
        }

        self.scopes
            .last_mut()
            .unwrap()
            .values
            .insert(String::from(name), Var::new(line));
    }

    fn define(&mut self, name: &str) {
        if self.scopes.len() == 0 {
            return;
        }

        let var = self
            .scopes
            .last_mut()
            .unwrap()
            .values
            .get_mut(name)
            .unwrap();

        var.define();
    }

    fn resolve_local(&mut self, name: &str, expression: &Expr, interpreter: &mut Interpreter) {
        let mut depth = 0;
        let mut scopes_iter = self.scopes.iter_mut();
        while let Some(scope) = scopes_iter.next_back() {
            if scope.values.contains_key(name) {
                // use the variable
                scope.values.get_mut(name).unwrap().reference();
                interpreter.resolve(expression as *const Expr, depth);
                break;
            }
            depth = depth + 1;
        }
    }

    fn expression<T: ErrorReporter>(
        &mut self,
        expression: &Expr,
        interpreter: &mut Interpreter,
        error_reporter: &mut T,
    ) {
        match expression {
            Expr::Literal(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    if self.scopes.len() != 0
                        && self.scopes.last().unwrap().values.get(name).is_some()
                        && !self
                            .scopes
                            .last()
                            .unwrap()
                            .values
                            .get(name)
                            .unwrap()
                            .defined
                    {
                        error_reporter.report(Error::Static {
                            message: String::from(
                                "Can't reference a variable inside it's declaration",
                            ),
                            line: token.line,
                        });
                    }

                    self.resolve_local(name, expression, interpreter);
                }
                TokenKind::This => {
                    if !self.in_fun {
                        error_reporter.report(Error::Static {
                            message: String::from("'this' can only be inside function bodies"),
                            line: token.line,
                        })
                    } else {
                        let mut depth = 0;
                        let mut scopes_iter = self.scopes.iter();

                        while let Some(scope) = scopes_iter.next_back() {
                            if scope.kind == ScopeKind::Fun {
                                break;
                            }

                            depth += 1;
                        }

                        interpreter.resolve(expression as *const Expr, depth);
                    }
                }
                _ => {}
            },
            Expr::Binary(_token, expression_1, expression_2) => {
                self.expression(expression_1, interpreter, error_reporter);
                self.expression(expression_2, interpreter, error_reporter);
            }
            Expr::Unary(_token, expression) => {
                self.expression(expression, interpreter, error_reporter);
            }
            Expr::FunCall(_token, expression, arguments) => {
                self.expression(expression, interpreter, error_reporter);
                for argument in arguments {
                    self.expression(argument, interpreter, error_reporter);
                }
            }
            Expr::Lamda(token, parameters, body) => {
                let enclosing = self.in_fun;
                self.in_fun = true;
                self.push_scope(ScopeKind::Fun);

                for Token { kind, line: _ } in parameters.iter() {
                    match kind {
                        TokenKind::Identifier(name) => {
                            self.declare(name, token.line);
                            self.define(name);
                        }
                        _ => {}
                    }
                }

                for statement in body.iter() {
                    self.statement(statement, interpreter, error_reporter);
                }

                self.pop_scope(error_reporter);
                self.in_fun = enclosing;
            }
            Expr::Get(_token, expression, _member) => {
                self.expression(expression, interpreter, error_reporter);
            }
            Expr::Set(_token, expression_1, expression_2) => {
                self.expression(expression_1, interpreter, error_reporter);
                self.expression(expression_2, interpreter, error_reporter);
            }
        }
    }

    fn statement<T: ErrorReporter>(
        &mut self,
        statement: &Statement,
        interpreter: &mut Interpreter,
        error_reporter: &mut T,
    ) {
        match statement {
            Statement::Block(statements) => {
                self.push_scope(ScopeKind::Block);
                for statement in statements {
                    self.statement(statement, interpreter, error_reporter);
                }
                self.pop_scope(error_reporter);
            }
            Statement::VarDecl(token, name, initializer) => {
                self.declare(name, token.line);
                match initializer {
                    Some(expression) => self.expression(expression, interpreter, error_reporter),
                    None => {}
                }
                self.define(name);
            }
            Statement::Fun(token, name, parameters, body) => {
                self.declare(name, token.line);
                self.define(name);
                let enclosing = self.in_fun.clone();
                self.in_fun = true;
                self.push_scope(ScopeKind::Fun);

                for Token { kind, line } in parameters.iter() {
                    match kind {
                        TokenKind::Identifier(name) => {
                            self.declare(name, *line);
                            self.define(name);
                        }
                        _ => {}
                    }
                }

                for statement in body.iter() {
                    self.statement(statement, interpreter, error_reporter);
                }

                self.pop_scope(error_reporter);
                self.in_fun = enclosing;
            }
            Statement::Expr(expression) => {
                self.expression(expression, interpreter, error_reporter);
            }
            Statement::Return(token, expression) => {
                if !self.in_fun {
                    error_reporter.report(Error::Static {
                        message: String::from("Can't return outside a function"),
                        line: token.line,
                    });
                }

                match expression {
                    Some(expression) => {
                        self.expression(expression, interpreter, error_reporter);
                    }
                    None => {}
                }
            }
            Statement::If(condition, then_branch, else_branch) => {
                self.expression(condition, interpreter, error_reporter);
                self.statement(then_branch, interpreter, error_reporter);
                match else_branch {
                    Some(else_branch) => self.statement(else_branch, interpreter, error_reporter),
                    None => {}
                }
            }
            Statement::For(initializer, condition, increment, body) => {
                self.push_scope(ScopeKind::Block);
                match initializer {
                    Some(initializer) => self.statement(initializer, interpreter, error_reporter),
                    None => {}
                }
                match condition {
                    Some(condition) => self.expression(condition, interpreter, error_reporter),
                    None => {}
                }
                match increment {
                    Some(increment) => self.expression(increment, interpreter, error_reporter),
                    None => {}
                }
                self.statement(body, interpreter, error_reporter);
                self.pop_scope(error_reporter);
            }
            Statement::While(condition, body) => {
                self.expression(condition, interpreter, error_reporter);
                self.statement(body, interpreter, error_reporter);
            }
            Statement::Class(_token, _name, methods) => {
                for method in methods.iter() {
                    self.statement(method, interpreter, error_reporter);
                }
            }
        }
    }

    pub fn resolve<T: ErrorReporter>(
        &mut self,
        interpreter: &mut Interpreter,
        error_reporter: &mut T,
    ) {
        for statement in self.program {
            self.statement(statement, interpreter, error_reporter);
        }
    }
}
