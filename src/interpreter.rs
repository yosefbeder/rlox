use super::environment::{DataType, Environment};
use super::error::{Error, ErrorReporter};
use super::parser::{Expr, Statement};
use super::scanner::{Token, TokenKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::Instant;

#[derive(Clone, Debug)]
pub enum Fun {
    Print,
    Clock,
    User {
        parameters: Rc<Vec<Token>>,
        body: Rc<Vec<Statement>>,
        closure: Rc<RefCell<Environment>>,
        is_initializer: bool,
    },
}

impl Fun {
    fn new(
        parameters: Rc<Vec<Token>>,
        body: Rc<Vec<Statement>>,
        closure: Rc<RefCell<Environment>>,
        is_initializer: bool,
    ) -> Self {
        Self::User {
            parameters,
            body,
            closure,
            is_initializer,
        }
    }

    fn call(&self, arguments: Vec<DataType>, interpreter: &Interpreter) -> Result<DataType, Error> {
        match self {
            Self::Print => {
                println!("{}", arguments[0].to_string());
                Ok(DataType::Nil)
            }
            Self::Clock => Ok(DataType::Number(
                interpreter.start_time.elapsed().as_millis() as f64,
            )),
            Self::User {
                parameters,
                body,
                closure,
                is_initializer,
            } => {
                let environment = Environment::new(Some(Rc::clone(closure)));

                let zip = parameters.iter().zip(arguments.iter());

                for (Token { kind, line: _ }, argument) in zip {
                    environment
                        .borrow_mut()
                        .define(
                            match kind {
                                TokenKind::Identifier(name) => name,
                                _ => "",
                            },
                            argument.clone(),
                        )
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

                if *is_initializer {
                    return Ok(closure.borrow().get_at("this", 0).unwrap());
                }

                Ok(DataType::Nil)
            }
        }
    }

    fn bind(&self, instance: Rc<RefCell<Instance>>) -> Fun {
        match self {
            Self::User {
                parameters,
                body,
                closure,
                is_initializer,
            } => {
                let environment = Environment::new(Some(Rc::clone(closure)));

                environment
                    .borrow_mut()
                    .define("this", DataType::Instance(instance))
                    .unwrap();

                Self::new(
                    Rc::clone(parameters),
                    Rc::clone(body),
                    environment,
                    *is_initializer,
                )
            }
            _ => unimplemented!(),
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
                is_initializer: _,
            } => parameters.len(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    pub name: String,
    members: HashMap<String, Fun>,
}

impl Class {
    fn new(name: &str, members: HashMap<String, Fun>) -> Self {
        Self {
            name: String::from(name),
            members,
        }
    }

    fn get(&self, name: &str) -> Option<Fun> {
        match self.members.get(name) {
            Some(member) => Some(member.clone()),
            None => None,
        }
    }

    fn arty(&self) -> usize {
        match self.members.get("init") {
            Some(fun) => fun.arty(),
            None => 0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Instance {
    fields: HashMap<String, DataType>,
    pub class: Rc<Class>,
}

impl Instance {
    fn new(class: Rc<Class>) -> Self {
        Self {
            fields: HashMap::new(),
            class,
        }
    }

    fn get(instance: Rc<RefCell<Self>>, name: &str) -> Option<DataType> {
        match instance.borrow().fields.get(name) {
            Some(value) => Some(value.clone()),
            None => match instance.borrow().class.get(name) {
                Some(fun) => Some(DataType::Fun(Rc::new(fun.bind(Rc::clone(&instance))))),
                None => None,
            },
        }
    }

    fn set(&mut self, name: &str, value: DataType) {
        self.fields.insert(String::from(name), value);
    }
}

impl Expr {
    fn get_line(&self) -> usize {
        match self {
            Expr::Literal(token) => token.line,
            Expr::Unary(operator, _expression) => operator.line,
            Expr::Binary(operator, _expression_1, _expression_2) => operator.line,
            Expr::FunCall(token, _callee, _arguments) => token.line,
            Expr::Lamda(token, _parameters, _body) => token.line,
            Expr::Get(token, _expression, _member) => token.line,
            Expr::Set(token, _expression_1, _expression_2) => token.line,
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
            .define("print", DataType::Fun(Rc::new(Fun::Print)))
            .unwrap();
        globals
            .borrow_mut()
            .define("clock", DataType::Fun(Rc::new(Fun::Clock)))
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
                let fun = Fun::new(
                    Rc::clone(parameters),
                    Rc::clone(body),
                    Rc::clone(&environment),
                    false,
                );

                match environment
                    .borrow_mut()
                    .define(name, DataType::Fun(Rc::new(fun)))
                {
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
            Statement::Class(_token, name, _parent, methods) => {
                let mut methods_map = HashMap::new();

                for statement in methods.iter() {
                    match statement {
                        Statement::Fun(_token, name, parameters, body) => {
                            methods_map.insert(
                                String::from(name),
                                Fun::new(
                                    Rc::clone(parameters),
                                    Rc::clone(body),
                                    Rc::clone(&environment),
                                    name == "init",
                                ),
                            );
                        }
                        _ => (),
                    };
                }

                environment
                    .borrow_mut()
                    .define(
                        name,
                        DataType::Class(Rc::new(Class::new(name, methods_map))),
                    )
                    .unwrap();

                Ok(None)
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
                TokenKind::This => {
                    let depth = self.locals.get(&(expression as *const Expr));

                    match depth {
                        Some(depth) => environment.borrow().get_at("this", *depth).unwrap(),
                        None => match self.globals.borrow().get("this") {
                            Some(value) => value,
                            None => {
                                return Err(Error::Runtime {
                                    message: format!("{} is undefined", "this"),
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
                        (DataType::Number(left), DataType::String(right)) => {
                            Ok(DataType::String(format!("{}{}", left, right)))
                        }
                        (DataType::String(left), DataType::Number(right)) => {
                            Ok(DataType::String(format!("{}{}", left, right)))
                        }
                        (DataType::Number(left), DataType::Number(right)) => {
                            Ok(DataType::Number(left + right))
                        }
                        _ => Err(Error::Runtime {
                            message: String::from("Each operand should be a number or string"),
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
            Expr::FunCall(token, callee, arguments) => {
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

                // Checking whether it's callable
                match interpreted_callee {
                    DataType::Fun(_) | DataType::Class(_) => {}
                    _ => {
                        return Err(Error::Runtime {
                            message: String::from("Callee didn't evaluate to a callable"),
                            line: token.line,
                        })
                    }
                }

                // Checking arty
                let arguments_count = interpreted_arguments.len();
                let arty = match &interpreted_callee {
                    DataType::Fun(fun) => fun.arty(),
                    DataType::Class(class) => class.arty(),
                    _ => unimplemented!(),
                };

                if arty != arguments_count {
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

                // Calling
                match &interpreted_callee {
                    DataType::Fun(fun) => fun.call(interpreted_arguments, self),
                    DataType::Class(class) => {
                        let instance = Instance::new(Rc::clone(class));

                        match class.get("init") {
                            Some(fun) => fun
                                .bind(Rc::new(RefCell::new(instance)))
                                .call(interpreted_arguments, self),
                            None => Ok(DataType::Instance(Rc::new(RefCell::new(instance)))),
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Expr::Lamda(_token, parameters, body) => {
                let fun = Fun::new(
                    Rc::clone(parameters),
                    Rc::clone(body),
                    Rc::clone(&environment),
                    false,
                );
                Ok(DataType::Fun(Rc::new(fun)))
            }
            Expr::Get(_token, expression, property) => {
                let line = property.line;
                let object = self.expression(expression, Rc::clone(&environment))?;

                let instance = match &object {
                    DataType::Instance(instance) => instance,
                    _ => {
                        return Err(Error::Runtime {
                            message: String::from("Only instances have properties"),
                            line: expression.get_line(),
                        })
                    }
                };
                let property = match &property.kind {
                    TokenKind::Identifier(name) => name,
                    _ => unimplemented!(),
                };
                let value = match Instance::get(Rc::clone(instance), property) {
                    Some(value) => value,
                    None => {
                        return Err(Error::Runtime {
                            message: format!("{} property is undefined", property),
                            line,
                        })
                    }
                };
                Ok(value)
            }
            Expr::Set(_token, expression, value) => {
                let value = self.expression(value, Rc::clone(&environment))?;

                match &**expression {
                    Expr::Get(token, expression, property) => {
                        let property = match &property.kind {
                            TokenKind::Identifier(name) => name,
                            _ => "",
                        };

                        match self.expression(expression, Rc::clone(&environment))? {
                            DataType::Instance(instance) => {
                                instance.borrow_mut().set(property, value.clone());
                                Ok(value)
                            }
                            _ => {
                                return Err(Error::Runtime {
                                    message: String::from("Only instances have properties"),
                                    line: token.line,
                                })
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            }
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
