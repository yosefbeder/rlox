use super::scanner::Token;
use super::scanner::TokenKind;
use super::Error;

/*
    GRAMMAR
        program -> statement*
        declaration -> var-declaration | statement
        var-declaration -> "var" IDENTIFIER ("=" expression)? ";"
        statement -> print-statement | expression-statement | block | if-statement | while-statement | for-statement
        while-statement -> "while" "(" expression ")" statement
        for-statement -> "for" "(" (";" | var-declaration | expression-statement) (expression? ";") expression? ")" statement
        block -> "{" declaration* "}"
        print-statement -> "print" expression ";"
        expression-statement -> expression ";"
        if-statement -> "if" "(" expression ")" statement ("else" statement)?
        expression -> assignment
        assignment -> IDENTIFIER "=" assignment | comma
        comma -> or ("," or)*
        or -> and ("or" and)*
        and -> equality ("and" equality)*
        equality -> comparison (("==" | "!=") comparison)*
        comparison -> term ((">" | ">=" | "<" | "<=") term)*
        term -> factor (("+" | "-") factor)*
        factor -> unary (("*" | "/") unary)*
        unary -> ("-" | "!") unary | primary
        primary -> STRING | NUMBER | IDENTIFIER | TRUE | FALSE | NIL | "(" expression ")"
*/

impl Token {
    fn is_literal(&self) -> bool {
        match self.kind {
            TokenKind::String(_)
            | TokenKind::Number(_)
            | TokenKind::Identifier(_)
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Nil => true,
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

#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(Token),
    Unary(Token, Box<Expr>),
    Binary(Token, Box<Expr>, Box<Expr>),
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Print(Expr),
    Expr(Expr),
    VarDecl(usize, String, Option<Expr>),
    Block(Vec<Box<Statement>>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    For(
        Option<Box<Statement>>,
        Option<Expr>,
        Option<Expr>,
        Box<Statement>,
    ),
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
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
        let token = self.next().unwrap();

        if token.is_literal() {
            Ok(Expr::Literal(token.clone()))
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

    fn unary(&mut self) -> Result<Expr, Error> {
        let token = self.peek().unwrap();

        if token.is_unary_operator() {
            Ok(Expr::Unary(
                self.next().unwrap().clone(),
                Box::new(self.unary()?),
            ))
        } else {
            Ok(self.primary()?)
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

    fn comma(&mut self) -> Result<Expr, Error> {
        let mut comma = self.or()?;

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Comma => {
                    comma = Expr::Binary(
                        self.next().unwrap().clone(),
                        Box::new(comma),
                        Box::new(self.or()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }

        Ok(comma)
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let comma = self.comma()?;

        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Equal {
                Ok(Expr::Binary(
                    self.next().unwrap().clone(),
                    Box::new(comma),
                    Box::new(self.assignment()?),
                ))
            } else {
                Ok(comma)
            }
        } else {
            Ok(comma)
        }
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        let assignment = self.assignment()?;

        Ok(assignment)
    }

    fn print_statement(&mut self) -> Result<Statement, Error> {
        let expression = self.expression()?;
        self.consume(
            TokenKind::Semicolon,
            "Expected a semi-colon at the end of the statement",
        )?;
        Ok(Statement::Print(expression))
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

    fn statement(&mut self) -> Result<Statement, Error> {
        if self.next_if_match(TokenKind::Print) {
            Ok(self.print_statement()?)
        } else if self.next_if_match(TokenKind::LeftBrace) {
            Ok(self.block()?)
        } else if self.next_if_match(TokenKind::If) {
            Ok(self.if_statement()?)
        } else if self.next_if_match(TokenKind::While) {
            Ok(self.while_statement()?)
        } else if self.next_if_match(TokenKind::For) {
            Ok(self.for_statement()?)
        } else {
            Ok(self.expression_statement()?)
        }
    }

    fn var_declaration(&mut self) -> Result<Statement, Error> {
        let line = self.peek_back().line;

        let name_err = Error::Syntax {
            message: String::from("Expected an identifer after the 'var' keyword"),
            line,
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
                    Ok(Statement::VarDecl(line, name, Some(expression)))
                }
                _ => {
                    self.consume(
                        TokenKind::Semicolon,
                        "Expected a semi-colon at the end of the statement",
                    )?;
                    Ok(Statement::VarDecl(line, name, None))
                }
            },
            _ => Err(Error::Syntax {
                message: String::from("Expected a semi-colon at the end of the statement"),
                line: self.get_last_line(),
            }),
        }
    }

    fn declaration(&mut self) -> Result<Statement, Error> {
        if self.next_if_match(TokenKind::Var) {
            Ok(self.var_declaration()?)
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
            declarations.push(Box::new(self.declaration()?));
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
                TokenKind::Print,
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

    pub fn parse(&mut self) -> Result<Vec<Statement>, Vec<Error>> {
        let mut statements = vec![];
        let mut errs = vec![];

        while let Some(token) = self.peek() {
            if token.kind == TokenKind::End {
                break;
            };
            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    errs.push(err);
                    self.synchronize();
                }
            }
        }

        match self.consume(TokenKind::End, "Expected the end of the file") {
            Ok(_) => {}
            Err(err) => errs.push(err),
        };

        if errs.len() > 0 {
            Err(errs)
        } else {
            Ok(statements)
        }
    }
}
