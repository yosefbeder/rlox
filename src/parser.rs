use super::scanner::Token;
use super::scanner::TokenKind;
use super::Error;

/*
    GRAMMAR
        program -> statement*
        declaration -> var-declaration | statement
        var-declaration -> "var" IDENTIFIER ("=" expression)? ";"
        statement -> print-statement | expression-statement
        print-statement -> "print" expression ";"
        expression-statement -> expression ";"
        expression -> assignment
        assignment -> IDENTIFIER "=" assignment | comma
        comma -> equality ("," equality)*
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
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn next(&mut self) -> Option<&Token> {
        let current_token = self.tokens.get(self.current);
        self.current += 1;
        current_token
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let token = self.next().unwrap();

        if let Ok(literal) = Literal::try_from(token.kind.clone()) {
            Ok(Expr::Literal(token.line, literal))
        } else {
            match token.kind {
                TokenKind::LeftParen => {
                    let expression = self.expression()?;
                    let err = Error::Syntax {
                        message: String::from("Expected a closing parenthese"),
                        line: expression.get_line(),
                    };

                    if let Some(token) = self.next() {
                        match token.kind {
                            TokenKind::RightParen => Ok(expression),
                            _ => Err(err),
                        }
                    } else {
                        Err(err)
                    }
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

    fn comma(&mut self) -> Result<Expr, Error> {
        let mut comma = self.equality()?;

        while let Some(token) = self.peek() {
            if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
                match binary_operator {
                    BinaryOperator::Comma => {
                        let line = token.line;
                        self.next();
                        let equality = self.equality()?;

                        comma = Expr::Binary(
                            line,
                            binary_operator,
                            Box::new(comma),
                            Box::new(equality),
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
        self.next();
        let expression = self.expression()?;
        let err = Error::Syntax {
            message: String::from("Expected ';' at the end of the statement"),
            line: expression.get_line(),
        };
        if let Some(token) = self.next() {
            if token.kind == TokenKind::Semicolon {
                Ok(Statement::Print(expression))
            } else {
                Err(err)
            }
        } else {
            Err(err)
        }
    }

    fn expression_statement(&mut self) -> Result<Statement, Error> {
        let expression = self.expression()?;
        let err = Error::Syntax {
            message: String::from("Expected ';' at the end of the statement"),
            line: expression.get_line(),
        };
        if let Some(token) = self.next() {
            if token.kind == TokenKind::Semicolon {
                Ok(Statement::Expr(expression))
            } else {
                Err(err)
            }
        } else {
            Err(err)
        }
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        let token = self.peek().unwrap();

        if token.kind == TokenKind::Print {
            Ok(self.print_statement()?)
        } else {
            Ok(self.expression_statement()?)
        }
    }

    fn var_declaration(&mut self) -> Result<Statement, Error> {
        let var_token = self.next().unwrap();
        let line = var_token.line;

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

        let semicolon_err = Error::Syntax {
            message: String::from("Expected ';' at the end of the statement"),
            line,
        };

        // checking whether there's an initializer or not
        match self.next() {
            Some(token) => match token.kind {
                TokenKind::Equal => {
                    let expression = self.expression()?;
                    match self.next() {
                        Some(token) => match token.kind {
                            TokenKind::Semicolon => {
                                Ok(Statement::VarDecl(line, name.clone(), Some(expression)))
                            }
                            _ => Err(semicolon_err),
                        },
                        _ => Err(semicolon_err),
                    }
                }
                TokenKind::Semicolon => Ok(Statement::VarDecl(line, name.clone(), None)),
                _ => Err(semicolon_err),
            },
            _ => Err(semicolon_err),
        }
    }

    fn declaration(&mut self) -> Result<Statement, Error> {
        let token = self.peek().unwrap();

        if token.kind == TokenKind::Var {
            Ok(self.var_declaration()?)
        } else {
            Ok(self.statement()?)
        }
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
            if token.kind != TokenKind::End {
                match self.declaration() {
                    Ok(statement) => statements.push(statement),
                    Err(err) => {
                        errs.push(err);
                        self.synchronize();
                    }
                }
            } else {
                break;
            }
        }

        if errs.len() > 1 {
            Err(errs)
        } else {
            let err = Error::Syntax {
                message: String::from("Expected the end of the file"),
                line: match statements.iter().last() {
                    Some(statement) => match statement {
                        Statement::Expr(expr) => expr.get_line(),
                        Statement::Print(expr) => expr.get_line(),
                        Statement::VarDecl(line, _name, _expr) => *line,
                    },
                    _ => 1,
                },
            };
            if let Some(token) = self.next() {
                if token.kind == TokenKind::End {
                    Ok(statements)
                } else {
                    errs.push(err);
                    Err(errs)
                }
            } else {
                errs.push(err);
                Err(errs)
            }
        }
    }
}
