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

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
    Identifier(String),
    True,
    False,
    Nil,
}

impl Literal {
    pub fn to_string(&self) -> String {
        match self {
            Self::String(value) => format!("\"{}\"", value),
            Self::Number(value) => format!("{}", value),
            Self::Identifier(value) => format!("{}", value),
            Self::True => format!("true"),
            Self::False => format!("false"),
            Self::Nil => format!("nil"),
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

impl TryFrom<TokenKind> for Literal {
    type Error = String;

    fn try_from(token: TokenKind) -> Result<Self, Self::Error> {
        match token {
            TokenKind::String(value) => Ok(Self::String(value)),
            TokenKind::Number(value) => Ok(Self::Number(value)),
            TokenKind::Identifier(value) => Ok(Self::Identifier(value)),
            TokenKind::True => Ok(Self::True),
            TokenKind::False => Ok(Self::False),
            TokenKind::Nil => Ok(Self::Nil),
            _ => Err(format!("Couldn't convert {:?} token to literal", token)),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl TryFrom<TokenKind> for UnaryOperator {
    type Error = String;

    fn try_from(token: TokenKind) -> Result<Self, Self::Error> {
        match token {
            TokenKind::Bang => Ok(Self::Bang),
            TokenKind::Minus => Ok(Self::Minus),
            _ => Err(format!(
                "Couldn't convert {:?} token to a unary operator",
                token
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Comma,
    And,
    Or,
}

impl TryFrom<TokenKind> for BinaryOperator {
    type Error = String;

    fn try_from(token: TokenKind) -> Result<Self, Self::Error> {
        match token {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Star => Ok(Self::Star),
            TokenKind::Slash => Ok(Self::Slash),
            TokenKind::Equal => Ok(Self::Equal),
            TokenKind::BangEqual => Ok(Self::BangEqual),
            TokenKind::EqualEqual => Ok(Self::EqualEqual),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEqual => Ok(Self::GreaterEqual),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::LessEqual => Ok(Self::LessEqual),
            TokenKind::Comma => Ok(Self::Comma),
            TokenKind::And => Ok(Self::And),
            TokenKind::Or => Ok(Self::Or),
            _ => Err(format!(
                "Couldn't convert {:?} token to a binary operator",
                token
            )),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(usize, Literal),
    Unary(usize, UnaryOperator, Box<Expr>),
    Binary(usize, BinaryOperator, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn get_line(&self) -> usize {
        match self {
            Self::Literal(line, _literal) => *line,
            Self::Binary(line, _operator, _expr_1, _expr_2) => *line,
            Self::Unary(line, _operator, _expr) => *line,
        }
    }
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
        let current_token = self.tokens.get(self.current);
        self.current += 1;
        current_token
    }

    fn next_if_match(&mut self, token: TokenKind) -> bool {
        if let Some(next_token) = self.peek() {
            if next_token.kind == token {
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

    fn consume(&mut self, token: TokenKind, message: &'static str) -> Result<(), Error> {
        if self.next_if_match(token) {
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

        if let Ok(literal) = Literal::try_from(token.kind.clone()) {
            Ok(Expr::Literal(token.line, literal))
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

        if let Ok(unary_operator) = UnaryOperator::try_from(token.kind.clone()) {
            let line = token.line;
            self.next();
            Ok(Expr::Unary(line, unary_operator, Box::new(self.unary()?)))
        } else {
            Ok(self.primary()?)
        }
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut factor = self.unary()?;

        while let Some(token) = self.peek() {
            if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
                match binary_operator {
                    BinaryOperator::Star => {
                        let line = token.line;
                        self.next();
                        let unary = self.unary()?;

                        factor =
                            Expr::Binary(line, binary_operator, Box::new(factor), Box::new(unary));
                    }
                    BinaryOperator::Slash => {
                        let line = token.line;
                        self.next();
                        let unary = self.unary()?;

                        factor =
                            Expr::Binary(line, binary_operator, Box::new(factor), Box::new(unary));
                    }
                    _ => {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(factor)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut term = self.factor()?;

        while let Some(token) = self.peek() {
            if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
                match binary_operator {
                    BinaryOperator::Plus => {
                        let line = token.line;
                        self.next();
                        let factor = self.factor()?;

                        term =
                            Expr::Binary(line, binary_operator, Box::new(term), Box::new(factor));
                    }
                    BinaryOperator::Minus => {
                        let line = token.line;
                        self.next();
                        let factor = self.factor()?;

                        term =
                            Expr::Binary(line, binary_operator, Box::new(term), Box::new(factor));
                    }
                    _ => {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(term)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut comparison = self.term()?;

        while let Some(token) = self.peek() {
            if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
                match binary_operator {
                    BinaryOperator::Greater => {
                        let line = token.line;
                        self.next();
                        let term = self.term()?;

                        comparison = Expr::Binary(
                            line,
                            binary_operator,
                            Box::new(comparison),
                            Box::new(term),
                        );
                    }
                    BinaryOperator::GreaterEqual => {
                        let line = token.line;
                        self.next();
                        let term = self.term()?;

                        comparison = Expr::Binary(
                            line,
                            binary_operator,
                            Box::new(comparison),
                            Box::new(term),
                        );
                    }
                    BinaryOperator::Less => {
                        let line = token.line;
                        self.next();
                        let term = self.term()?;

                        comparison = Expr::Binary(
                            line,
                            binary_operator,
                            Box::new(comparison),
                            Box::new(term),
                        );
                    }
                    BinaryOperator::LessEqual => {
                        let line = token.line;
                        self.next();
                        let term = self.term()?;

                        comparison = Expr::Binary(
                            line,
                            binary_operator,
                            Box::new(comparison),
                            Box::new(term),
                        );
                    }
                    _ => {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(comparison)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut equality = self.comparison()?;

        while let Some(token) = self.peek() {
            if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
                match binary_operator {
                    BinaryOperator::BangEqual => {
                        let line = token.line;
                        self.next();
                        let comparison = self.comparison()?;

                        equality = Expr::Binary(
                            line,
                            binary_operator,
                            Box::new(equality),
                            Box::new(comparison),
                        );
                    }
                    BinaryOperator::EqualEqual => {
                        let line = token.line;
                        self.next();
                        let comparison = self.comparison()?;

                        equality = Expr::Binary(
                            line,
                            binary_operator,
                            Box::new(equality),
                            Box::new(comparison),
                        );
                    }
                    _ => {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(equality)
    }

    fn and(&mut self) -> Result<Expr, Error> {
        let mut and = self.equality()?;

        while self.next_if_match(TokenKind::And) {
            let line = self.peek_back().line;
            let equality = self.equality()?;

            and = Expr::Binary(line, BinaryOperator::And, Box::new(and), Box::new(equality));
        }

        Ok(and)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let mut or = self.and()?;

        while self.next_if_match(TokenKind::Or) {
            let line = self.peek_back().line;
            let and = self.and()?;

            or = Expr::Binary(line, BinaryOperator::Or, Box::new(or), Box::new(and));
        }

        Ok(or)
    }

    fn comma(&mut self) -> Result<Expr, Error> {
        let mut comma = self.or()?;

        while let Some(token) = self.peek() {
            if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
                match binary_operator {
                    BinaryOperator::Comma => {
                        let line = token.line;
                        self.next();
                        let or = self.or()?;

                        comma = Expr::Binary(line, binary_operator, Box::new(comma), Box::new(or));
                    }
                    _ => {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        Ok(comma)
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let comma = self.comma()?;
        if let Some(token) = self.peek() {
            if let TokenKind::Equal = &token.kind {
                let line = self.next().unwrap().line;
                let assignment = self.assignment()?;
                Ok(Expr::Binary(
                    line,
                    BinaryOperator::Equal,
                    Box::new(comma),
                    Box::new(assignment),
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

        println!(
            "{:?}",
            Statement::For(initializer, condition, increment, Box::new(body),)
        );

        Ok(Statement::Expr(Expr::Literal(1, Literal::False)))
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
