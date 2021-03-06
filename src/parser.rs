use super::error::Error;
use super::error::ErrorReporter;
use super::scanner::Token;
use super::scanner::TokenKind;
use std::rc::Rc;

/*
    GRAMMAR
        program -> statement*
        declaration -> var-declaration | fun-declaration | class-declaration | statement
        class-declaration -> "class" IDENTIFIER ("extends" IDENTIFIER)? "{" function* "}"
        fun-declaration -> "fun" function
        function -> IDENTIFIER "(" parameters? ")" block
        paramters -> IDENTIFIER ("," IDENTIFIER)*
        var-declaration -> "var" IDENTIFIER ("=" expression)? ";"
        statement -> expression-statement | block | if-statement | while-statement | for-statement | return-statement
        return-statement -> "return" expression? ";"
        while-statement -> "while" "(" expression ")" statement
        for-statement -> "for" "(" (";" | var-declaration | expression-statement) (expression? ";") expression? ")" statement
        block -> "{" declaration* "}"
        expression-statement -> expression ";"
        if-statement -> "if" "(" expression ")" statement ("else" statement)?
        expression -> lamda | comma
        lamda -> "fun" "(" parameters? ")" block
        comma -> assignment ("," assignment)*
        assignment -> (call ".")? IDENTIFIER "=" assignment | or
        or -> and ("or" and)*
        and -> equality ("and" equality)*
        equality -> comparison (("==" | "!=") comparison)*
        comparison -> term ((">" | ">=" | "<" | "<=") term)*
        term -> factor (("+" | "-") factor)*
        factor -> unary (("*" | "/") unary)*
        unary -> ("-" | "!") unary | call
        call -> primary ("(" arguments? ")" | "." IDENTIFIER)*
        arguments -> assignment ("," assignment)*
        primary -> STRING | NUMBER | IDENTIFIER | "true" | "false" | "nil" | "this" | "super" "." IDENTIFIER | "(" expression ")"
*/

impl Token {
    fn is_literal(&self) -> bool {
        match self.kind {
            TokenKind::String(_)
            | TokenKind::Number(_)
            | TokenKind::Identifier(_)
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Nil
            | TokenKind::This => true,
            _ => false,
        }
    }

    fn is_unary_operator(&self) -> bool {
        match self.kind {
            TokenKind::Bang | TokenKind::Minus => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Literal(Token),
    Unary(Token, Box<Expr>),
    Binary(Token, Box<Expr>, Box<Expr>),
    Lamda(Token, Rc<Vec<Token>>, Rc<Vec<Statement>>),
    FunCall(Token, Box<Expr>, Vec<Expr>),
    Get(Token, Box<Expr>, Token),
    Set(Token, Box<Expr>, Box<Expr>),
    Super(Token, Token),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    VarDecl(Token, String, Option<Expr>),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(
        Option<Box<Statement>>,
        Option<Expr>,
        Option<Expr>,
        Box<Statement>,
    ),
    Fun(Token, String, Rc<Vec<Token>>, Rc<Vec<Statement>>),
    Return(Token, Option<Expr>),
    Class(Token, String, Option<String>, Rc<Vec<Statement>>),
}

pub struct Parser<'a, 'b, T: ErrorReporter> {
    tokens: &'a [Token],
    error_reporter: &'b mut T,
    current: usize,
}

impl<'a, 'b, T: ErrorReporter> Parser<'a, 'b, T> {
    pub fn new(tokens: &'a [Token], error_reporter: &'b mut T) -> Self {
        Self {
            tokens,
            error_reporter,
            current: 0,
        }
    }

    fn get_last_line(&self) -> usize {
        self.tokens.iter().last().unwrap().line
    }

    fn next(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.current);
        self.current += 1;
        token
    }

    fn next_if_match(&mut self, token_kind: TokenKind) -> bool {
        if let Some(token) = self.peek() {
            if token.kind == token_kind {
                self.next();
                return true;
            }
        }

        return false;
    }

    fn peek_back(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn consume(&mut self, token_kind: TokenKind, message: &'static str) -> Result<(), Error> {
        if self.next_if_match(token_kind) {
            Ok(())
        } else {
            Err(Error::Syntax {
                message: String::from(message),
                line: self.peek_back().line,
            })
        }
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let token = self.next().unwrap().clone();

        if token.is_literal() {
            Ok(Expr::Literal(token.clone()))
        } else if token.kind == TokenKind::Super {
            let super_token = self.peek_back().clone();
            self.consume(TokenKind::Dot, "Expected a dot after 'super'")?;
            let error = Err(Error::Syntax {
                message: String::from("Expected an identifier after 'super.'"),
                line: self.peek_back().line,
            });

            match self.next() {
                Some(token) => match token.kind {
                    TokenKind::Identifier(_) => Ok(Expr::Super(super_token, token.clone())),
                    _ => error,
                },
                None => error,
            }
        } else {
            match token.kind {
                TokenKind::LeftParen => {
                    let expression = self.expression()?;
                    self.consume(TokenKind::RightParen, "Expected a closing parenthese ')'")?;
                    Ok(expression)
                }
                _ => Err(Error::Syntax {
                    message: String::from("Expected an expression"),
                    line: token.line,
                }),
            }
        }
    }

    fn arguments(&mut self) -> Result<Vec<Expr>, Error> {
        let mut arguments = vec![self.assignment()?];

        while self.next_if_match(TokenKind::Comma) {
            if arguments.len() >= 255 {
                self.error_reporter.report(Error::Syntax {
                    message: String::from("Can't have more than 255 arguments"),
                    line: self.peek().unwrap().line,
                });
            }
            arguments.push(self.assignment()?);
        }

        Ok(arguments)
    }

    fn call(&mut self) -> Result<Expr, Error> {
        let mut call = self.primary()?;

        loop {
            if self.next_if_match(TokenKind::LeftParen) {
                let token = self.peek_back().clone();

                if self.next_if_match(TokenKind::RightParen) {
                    call = Expr::FunCall(token, Box::new(call), vec![]);
                } else {
                    call = Expr::FunCall(token, Box::new(call), self.arguments()?);
                    self.consume(
                        TokenKind::RightParen,
                        "Expected a closing parenthese at the end of the function call",
                    )?;
                }
            } else if self.next_if_match(TokenKind::Dot) {
                let dot_token = self.peek_back().clone();

                if let Some(token) = self.peek() {
                    if let TokenKind::Identifier(_) = token.kind {
                        call = Expr::Get(dot_token, Box::new(call), self.next().unwrap().clone());
                        continue;
                    }
                }

                return Err(Error::Syntax {
                    message: String::from("Expected an identifier after the dot"),
                    line: dot_token.line,
                });
            } else {
                break;
            }
        }

        Ok(call)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        let token = self.peek().unwrap();

        if token.is_unary_operator() {
            Ok(Expr::Unary(
                self.next().unwrap().clone(),
                Box::new(self.unary()?),
            ))
        } else {
            Ok(self.call()?)
        }
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut factor = self.unary()?;

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Star | TokenKind::Slash => {
                    factor = Expr::Binary(
                        self.next().unwrap().clone(),
                        Box::new(factor),
                        Box::new(self.unary()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(factor)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut term = self.factor()?;

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    term = Expr::Binary(
                        self.next().unwrap().clone(),
                        Box::new(term),
                        Box::new(self.factor()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(term)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut comparison = self.term()?;

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Less
                | TokenKind::LessEqual => {
                    comparison = Expr::Binary(
                        self.next().unwrap().clone(),
                        Box::new(comparison),
                        Box::new(self.term()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(comparison)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut equality = self.comparison()?;

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::EqualEqual | TokenKind::BangEqual => {
                    equality = Expr::Binary(
                        self.next().unwrap().clone(),
                        Box::new(equality),
                        Box::new(self.comparison()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(equality)
    }

    fn and(&mut self) -> Result<Expr, Error> {
        let mut and = self.equality()?;

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::And => {
                    and = Expr::Binary(
                        self.next().unwrap().clone(),
                        Box::new(and),
                        Box::new(self.equality()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(and)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let mut or = self.and()?;

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Or => {
                    or = Expr::Binary(
                        self.next().unwrap().clone(),
                        Box::new(or),
                        Box::new(self.and()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(or)
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let or = self.or()?;

        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Equal {
                let token = self.next().unwrap().clone();

                match or {
                    Expr::Literal(Token {
                        kind: TokenKind::Identifier(_),
                        line: _,
                    }) => {}
                    Expr::Get(_, _, _) => {
                        return Ok(Expr::Set(token, Box::new(or), Box::new(self.assignment()?)))
                    }
                    _ => {
                        self.error_reporter.report(Error::Syntax {
                            message: String::from("Bad assingment taget"),
                            line: token.line,
                        });
                    }
                };

                Ok(Expr::Binary(
                    token,
                    Box::new(or),
                    Box::new(self.assignment()?),
                ))
            } else {
                Ok(or)
            }
        } else {
            Ok(or)
        }
    }

    fn comma(&mut self) -> Result<Expr, Error> {
        let mut comma = self.assignment()?;

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Comma => {
                    comma = Expr::Binary(
                        self.next().unwrap().clone(),
                        Box::new(comma),
                        Box::new(self.assignment()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(comma)
    }

    fn lamda(&mut self) -> Result<Expr, Error> {
        let token = self.peek_back().clone();

        self.consume(
            TokenKind::LeftParen,
            "Expected an opening parenthese after the 'fun' keyword",
        )?;

        let mut paramters = vec![];

        if !self.next_if_match(TokenKind::RightParen) {
            paramters = self.parameters()?;
            self.consume(
                TokenKind::RightParen,
                "Expected a closing parenthese after the paramters",
            )?;
        }

        self.consume(TokenKind::LeftBrace, "Expected a block after the paramters")?;

        let body = self.block()?;

        match body {
            Statement::Block(body) => Ok(Expr::Lamda(token, Rc::new(paramters), Rc::new(body))),
            _ => Err(Error::Syntax {
                message: String::from("Expected the body of the function to be a block"),
                line: token.line,
            }),
        }
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        if self.next_if_match(TokenKind::Fun) {
            Ok(self.lamda()?)
        } else {
            let comma = self.comma()?;
            Ok(comma)
        }
    }

    fn expression_statement(&mut self) -> Result<Statement, Error> {
        let expression = self.expression()?;
        self.consume(
            TokenKind::Semicolon,
            "Expected a semi-colon at the end of the statement",
        )?;
        Ok(Statement::Expr(expression))
    }

    fn if_statement(&mut self) -> Result<Statement, Error> {
        self.consume(
            TokenKind::LeftParen,
            "Expected an openning parenthese after the 'if' keyword",
        )?;
        let condition = self.expression()?;
        self.consume(
            TokenKind::RightParen,
            "Expected a closing parenthese after the condition",
        )?;
        let if_branch = self.statement()?;
        let mut else_branch = None;

        if self.next_if_match(TokenKind::Else) {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Statement::If(condition, Box::new(if_branch), else_branch))
    }

    fn while_statement(&mut self) -> Result<Statement, Error> {
        self.consume(
            TokenKind::LeftParen,
            "Expected an openning parenthese after the 'while' keyword",
        )?;
        let condition = self.expression()?;
        self.consume(
            TokenKind::RightParen,
            "Expected a closing parenthese after the condition",
        )?;
        let body = self.statement()?;

        Ok(Statement::While(condition, Box::new(body)))
    }

    fn for_statement(&mut self) -> Result<Statement, Error> {
        self.consume(
            TokenKind::LeftParen,
            "Expected an openning parenthese after the 'for' keyword",
        )?;

        let mut initializer = None;

        if self.next_if_match(TokenKind::Var) {
            initializer = Some(Box::new(self.var_declaration()?));
        } else if !self.next_if_match(TokenKind::Semicolon) {
            initializer = Some(Box::new(self.expression_statement()?));
        }

        let mut condition = None;

        if !self.next_if_match(TokenKind::Semicolon) {
            condition = Some(self.expression()?);
            self.consume(
                TokenKind::Semicolon,
                "Expected a semi-colon after the condition",
            )?;
        }

        let mut increment = None;

        if !self.next_if_match(TokenKind::RightParen) {
            increment = Some(self.expression()?);
            self.consume(
                TokenKind::RightParen,
                "Expected a closing parenthese before the body",
            )?;
        }

        let body = self.statement()?;

        Ok(Statement::For(
            initializer,
            condition,
            increment,
            Box::new(body),
        ))
    }

    fn return_statement(&mut self) -> Result<Statement, Error> {
        let token = self.peek_back().clone();

        let mut expression = None;

        if !self.next_if_match(TokenKind::Semicolon) {
            expression = Some(self.expression()?);
            self.consume(
                TokenKind::Semicolon,
                "Expected a semi-colon after the return statement",
            )?;
        }

        Ok(Statement::Return(token, expression))
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        if self.next_if_match(TokenKind::LeftBrace) {
            Ok(self.block()?)
        } else if self.next_if_match(TokenKind::If) {
            Ok(self.if_statement()?)
        } else if self.next_if_match(TokenKind::While) {
            Ok(self.while_statement()?)
        } else if self.next_if_match(TokenKind::For) {
            Ok(self.for_statement()?)
        } else if self.next_if_match(TokenKind::Return) {
            Ok(self.return_statement()?)
        } else {
            Ok(self.expression_statement()?)
        }
    }

    fn var_declaration(&mut self) -> Result<Statement, Error> {
        let var_token = self.peek_back().clone();

        let name_err = Error::Syntax {
            message: String::from("Expected an identifer after the 'var' keyword"),
            line: var_token.line,
        };

        let name = match self.next() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(value) => value.clone(),
                _ => return Err(name_err),
            },
            _ => return Err(name_err),
        };

        // checking whether there's an initializer or not
        match self.peek() {
            Some(token) => match token.kind {
                TokenKind::Equal => {
                    self.next();
                    let expression = self.expression()?;
                    self.consume(
                        TokenKind::Semicolon,
                        "Expected a semi-colon at the end of the statement",
                    )?;
                    Ok(Statement::VarDecl(var_token, name, Some(expression)))
                }
                _ => {
                    self.consume(
                        TokenKind::Semicolon,
                        "Expected a semi-colon at the end of the statement",
                    )?;
                    Ok(Statement::VarDecl(var_token, name, None))
                }
            },
            _ => Err(Error::Syntax {
                message: String::from("Expected a semi-colon at the end of the statement"),
                line: self.get_last_line(),
            }),
        }
    }

    fn fun_declaration(&mut self) -> Result<Statement, Error> {
        Ok(self.function()?)
    }

    fn function(&mut self) -> Result<Statement, Error> {
        let fun_token = self.peek_back().clone();

        let name_err = Error::Syntax {
            message: String::from("Expected an identifer"),
            line: fun_token.line,
        };

        let name = match self.next() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(value) => value.clone(),
                _ => return Err(name_err),
            },
            _ => return Err(name_err),
        };

        self.consume(
            TokenKind::LeftParen,
            "Expected an opening parenthese after the name of the function",
        )?;

        let mut paramters = vec![];

        if !self.next_if_match(TokenKind::RightParen) {
            paramters = self.parameters()?;
            self.consume(
                TokenKind::RightParen,
                "Expected a closing parenthese after the paramters",
            )?;
        }

        self.consume(TokenKind::LeftBrace, "Expected a block after the paramters")?;

        let body = self.block()?;

        match body {
            Statement::Block(body) => Ok(Statement::Fun(
                fun_token,
                name,
                Rc::new(paramters),
                Rc::new(body),
            )),
            _ => Err(Error::Syntax {
                message: String::from("Expected the body of the function to be a block"),
                line: fun_token.line,
            }),
        }
    }

    fn parameters(&mut self) -> Result<Vec<Token>, Error> {
        let mut result = vec![];

        if let Some(token) = self.peek() {
            if let TokenKind::Identifier(_) = &token.kind {
                result.push(self.next().unwrap().clone());
            } else {
                return Err(Error::Syntax {
                    message: String::from("Expected an identifier or a closing parenthese"),
                    line: self.peek_back().line,
                });
            }
        } else {
            return Err(Error::Syntax {
                message: String::from(
                    "Expected an identifier or a closing parenthese, but get nothing",
                ),
                line: self.get_last_line(),
            });
        }

        while self.next_if_match(TokenKind::Comma) {
            let error = Error::Syntax {
                message: String::from("Expected an identifier after the comma"),
                line: self.peek_back().line,
            };

            if let Some(token) = self.peek() {
                if let TokenKind::Identifier(_) = &token.kind {
                    result.push(self.next().unwrap().clone());
                } else {
                    return Err(error);
                }
            } else {
                return Err(error);
            }
        }

        Ok(result)
    }

    fn class_declaration(&mut self) -> Result<Statement, Error> {
        let class_token = self.peek_back().clone();

        let name_err = Error::Syntax {
            message: String::from("Expected an identifer after the 'class' keyword"),
            line: class_token.line,
        };

        let name = match self.next() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(value) => value.clone(),
                _ => return Err(name_err),
            },
            _ => return Err(name_err),
        };

        let parent = if self.next_if_match(TokenKind::Extends) {
            let error = Err(Error::Syntax {
                message: String::from("Expected the parent class name after 'extends'"),
                line: self.peek_back().line,
            });

            match self.next() {
                Some(token) => match &token.kind {
                    TokenKind::Identifier(value) => Some(value.to_owned()),
                    _ => return error,
                },
                None => return error,
            }
        } else {
            None
        };

        self.consume(TokenKind::LeftBrace, "Expected a block")?;

        let mut methods = vec![];

        while !self.next_if_match(TokenKind::RightBrace) {
            match self.peek() {
                Some(token) => match token.kind {
                    TokenKind::Identifier(_) => {
                        methods.push(self.function()?);
                    }
                    TokenKind::End => {
                        let line = token.line;

                        return Err(Error::Syntax {
                            message: String::from("Unterimated class"),
                            line,
                        });
                    }
                    TokenKind::Fun => {
                        let line = self.next().unwrap().line;
                        self.function()?;

                        self.error_reporter.report(Error::Syntax {
                            message: String::from("Methods are too classy to have fun"),
                            line,
                        });
                    }
                    TokenKind::Var => {
                        let line = self.next().unwrap().line;
                        self.var_declaration()?;

                        self.error_reporter.report(Error::Syntax {
                            message: String::from("You can't declare a variable inside a class"),
                            line,
                        });
                    }
                    _ => {
                        let line = token.line;
                        self.error_reporter.report(Error::Syntax {
                            message: String::from("Expected a function"),
                            line,
                        })
                    }
                },
                None => {}
            }
        }

        Ok(Statement::Class(
            class_token,
            name,
            parent,
            Rc::new(methods),
        ))
    }

    fn declaration(&mut self) -> Result<Statement, Error> {
        if self.next_if_match(TokenKind::Var) {
            Ok(self.var_declaration()?)
        } else if self.next_if_match(TokenKind::Fun) {
            Ok(self.fun_declaration()?)
        } else if self.next_if_match(TokenKind::Class) {
            Ok(self.class_declaration()?)
        } else {
            Ok(self.statement()?)
        }
    }

    fn block(&mut self) -> Result<Statement, Error> {
        let mut declarations = vec![];

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RightBrace || token.kind == TokenKind::End {
                break;
            }
            declarations.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, "Unterminated block")?;
        Ok(Statement::Block(declarations))
    }

    fn synchronize(&mut self) {
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::Semicolon {
                self.next();
                break;
            }

            if [
                TokenKind::Class,
                TokenKind::Else,
                TokenKind::Fun,
                TokenKind::For,
                TokenKind::If,
                TokenKind::Return,
                TokenKind::Super,
                TokenKind::This,
                TokenKind::Var,
                TokenKind::While,
                TokenKind::End,
            ]
            .contains(&token.kind)
            {
                break;
            }

            self.next();
        }
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = vec![];

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::End {
                break;
            };
            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    self.error_reporter.report(err);
                    self.synchronize();
                }
            }
        }

        match self.consume(TokenKind::End, "Expected the end of the file") {
            Ok(_) => {}
            Err(err) => self.error_reporter.report(err),
        };

        statements
    }
}
