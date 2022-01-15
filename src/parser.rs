use super::errors::RuntimeError;
use super::errors::SyntaxError;
use super::scanner::Token;
use super::scanner::TokenKind;
use std::convert::TryFrom;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::slice::Iter;

/*
    GRAMMAR
        program -> statement*
        statement -> print-statement | expression-statement
        print-statement -> "print" expression ";"
        expression-statement -> expression ";"
        expression -> comma
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
    fn to_string(&self) -> String {
        match self {
            Self::String(value) => format!("\"{}\"", value),
            Self::Number(value) => format!("{}", value),
            Self::Identifier(value) => format!("{}", value),
            Self::True => format!("true"),
            Self::False => format!("false"),
            Self::Nil => format!("nil"),
        }
    }

    fn is_truthy(&self) -> bool {
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

impl UnaryOperator {
    fn to_string(&self) -> &'static str {
        match self {
            Self::Minus => "-",
            Self::Bang => "!",
        }
    }
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
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Comma,
}

impl BinaryOperator {
    fn to_string(&self) -> &'static str {
        match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::BangEqual => "!=",
            Self::EqualEqual => "==",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::Comma => ",",
        }
    }
}

impl TryFrom<TokenKind> for BinaryOperator {
    type Error = String;

    fn try_from(token: TokenKind) -> Result<Self, Self::Error> {
        match token {
            TokenKind::Plus => Ok(Self::Plus),
            TokenKind::Minus => Ok(Self::Minus),
            TokenKind::Star => Ok(Self::Star),
            TokenKind::Slash => Ok(Self::Slash),
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
    fn to_string(&self) -> String {
        match self {
            Self::Literal(_line, literal) => literal.to_string(),
            Self::Binary(_line, operator, expr_1, expr_2) => {
                format!(
                    "({}, {}, {})",
                    operator.to_string(),
                    expr_1.to_string(),
                    expr_2.to_string(),
                )
            }
            Self::Unary(_line, operator, expr) => {
                format!("({}, {})", operator.to_string(), expr.to_string())
            }
        }
    }

    fn get_line(&self) -> usize {
        match self {
            Self::Literal(line, _literal) => *line,
            Self::Binary(line, _operator, _expr_1, _expr_2) => *line,
            Self::Unary(line, _operator, _expr) => *line,
        }
    }

    fn eval(&self) -> Result<Literal, RuntimeError> {
        match self {
            Self::Literal(_line, literal) => Ok(literal.clone()),
            Self::Unary(line, operator, expr) => {
                let right = expr.eval()?;

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
            Self::Binary(line, operator, expr_1, expr_2) => {
                let left = expr_1.eval()?;
                let right = expr_2.eval()?;

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
                }
            }
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Print(Expr),
    Expr(Expr),
}

fn parse_primary(tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>) -> Result<Expr, SyntaxError> {
    let (_, token) = tokens_iter.next().unwrap();

    if let Ok(literal) = Literal::try_from(token.kind.clone()) {
        Ok(Expr::Literal(token.line, literal))
    } else {
        match token.kind {
            TokenKind::LeftParen => {
                let expression = parse_expression(tokens_iter)?;
                let err = SyntaxError::new(
                    String::from("Expected a closing parenthese"),
                    expression.get_line(),
                );

                if let Some((_, token)) = tokens_iter.next() {
                    match token.kind {
                        TokenKind::RightParen => Ok(expression),
                        _ => Err(err),
                    }
                } else {
                    Err(err)
                }
            }
            _ => Err(SyntaxError::new(
                String::from("Expected an expression"),
                token.line,
            )),
        }
    }
}

fn parse_unary(tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>) -> Result<Expr, SyntaxError> {
    let (_, token) = tokens_iter.peek().unwrap();

    if let Ok(unary_operator) = UnaryOperator::try_from(token.kind.clone()) {
        let line = token.line;
        tokens_iter.next();
        Ok(Expr::Unary(
            line,
            unary_operator,
            Box::new(parse_unary(tokens_iter)?),
        ))
    } else {
        Ok(parse_primary(tokens_iter)?)
    }
}

fn parse_factor(tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>) -> Result<Expr, SyntaxError> {
    let mut factor = parse_unary(tokens_iter)?;

    while let Some((_, token)) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::Star => {
                    let line = token.line;
                    tokens_iter.next();
                    let unary = parse_unary(tokens_iter)?;

                    factor = Expr::Binary(line, binary_operator, Box::new(factor), Box::new(unary));
                }
                BinaryOperator::Slash => {
                    let line = token.line;
                    tokens_iter.next();
                    let unary = parse_unary(tokens_iter)?;

                    factor = Expr::Binary(line, binary_operator, Box::new(factor), Box::new(unary));
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

fn parse_term(tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>) -> Result<Expr, SyntaxError> {
    let mut term = parse_factor(tokens_iter)?;

    while let Some((_, token)) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::Plus => {
                    let line = token.line;
                    tokens_iter.next();
                    let factor = parse_factor(tokens_iter)?;

                    term = Expr::Binary(line, binary_operator, Box::new(term), Box::new(factor));
                }
                BinaryOperator::Minus => {
                    let line = token.line;
                    tokens_iter.next();
                    let factor = parse_factor(tokens_iter)?;

                    term = Expr::Binary(line, binary_operator, Box::new(term), Box::new(factor));
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

fn parse_comparison(
    tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>,
) -> Result<Expr, SyntaxError> {
    let mut comparison = parse_term(tokens_iter)?;

    while let Some((_, token)) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::Greater => {
                    let line = token.line;
                    tokens_iter.next();
                    let term = parse_term(tokens_iter)?;

                    comparison =
                        Expr::Binary(line, binary_operator, Box::new(comparison), Box::new(term));
                }
                BinaryOperator::GreaterEqual => {
                    let line = token.line;
                    tokens_iter.next();
                    let term = parse_term(tokens_iter)?;

                    comparison =
                        Expr::Binary(line, binary_operator, Box::new(comparison), Box::new(term));
                }
                BinaryOperator::Less => {
                    let line = token.line;
                    tokens_iter.next();
                    let term = parse_term(tokens_iter)?;

                    comparison =
                        Expr::Binary(line, binary_operator, Box::new(comparison), Box::new(term));
                }
                BinaryOperator::LessEqual => {
                    let line = token.line;
                    tokens_iter.next();
                    let term = parse_term(tokens_iter)?;

                    comparison =
                        Expr::Binary(line, binary_operator, Box::new(comparison), Box::new(term));
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

fn parse_equality(tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>) -> Result<Expr, SyntaxError> {
    let mut equality = parse_comparison(tokens_iter)?;

    while let Some((_, token)) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::BangEqual => {
                    let line = token.line;
                    tokens_iter.next();
                    let comparison = parse_comparison(tokens_iter)?;

                    equality = Expr::Binary(
                        line,
                        binary_operator,
                        Box::new(equality),
                        Box::new(comparison),
                    );
                }
                BinaryOperator::EqualEqual => {
                    let line = token.line;
                    tokens_iter.next();
                    let comparison = parse_comparison(tokens_iter)?;

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

fn parse_comma(tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>) -> Result<Expr, SyntaxError> {
    let mut comma = parse_equality(tokens_iter)?;

    while let Some((_, token)) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::Comma => {
                    let line = token.line;
                    tokens_iter.next();
                    let equality = parse_equality(tokens_iter)?;

                    comma =
                        Expr::Binary(line, binary_operator, Box::new(comma), Box::new(equality));
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

fn parse_expression(
    tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>,
) -> Result<Expr, SyntaxError> {
    let comma = parse_comma(tokens_iter)?;

    Ok(comma)
}

fn parse_print_statement(
    tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>,
) -> Result<Statement, SyntaxError> {
    tokens_iter.next();
    let expression = parse_expression(tokens_iter)?;
    let err = SyntaxError::new(
        String::from("Expected ';' at the end of the statement"),
        expression.get_line(),
    );
    if let Some((_, token)) = tokens_iter.next() {
        if token.kind == TokenKind::Semicolon {
            Ok(Statement::Print(expression))
        } else {
            Err(err)
        }
    } else {
        Err(err)
    }
}

fn parse_expression_statement(
    tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>,
) -> Result<Statement, SyntaxError> {
    let expression = parse_expression(tokens_iter)?;
    let err = SyntaxError::new(
        String::from("Expected ';' at the end of the statement"),
        expression.get_line(),
    );
    if let Some((_, token)) = tokens_iter.next() {
        if token.kind == TokenKind::Semicolon {
            Ok(Statement::Expr(expression))
        } else {
            Err(err)
        }
    } else {
        Err(err)
    }
}

fn parse_statement(
    tokens_iter: &mut Peekable<Enumerate<Iter<Token>>>,
) -> Result<Statement, SyntaxError> {
    let (_, token) = tokens_iter.peek().unwrap();

    if token.kind == TokenKind::Print {
        Ok(parse_print_statement(tokens_iter)?)
    } else {
        Ok(parse_expression_statement(tokens_iter)?)
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Statement>, SyntaxError> {
    let mut tokens_iter = tokens.iter().enumerate().peekable();
    let mut statements = vec![];

    while let Some((_, token)) = tokens_iter.peek() {
        if token.kind != TokenKind::End {
            statements.push(parse_statement(&mut tokens_iter)?);
        } else {
            break;
        }
    }

    Ok(statements)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_to_string() {
        let expr = Expr::Binary(
            1,
            BinaryOperator::LessEqual,
            Box::new(Expr::Binary(
                1,
                BinaryOperator::Plus,
                Box::new(Expr::Literal(1, Literal::String(String::from("hi")))),
                Box::new(Expr::Unary(
                    1,
                    UnaryOperator::Minus,
                    Box::new(Expr::Literal(1, Literal::Identifier(String::from("x")))),
                )),
            )),
            Box::new(Expr::Binary(
                1,
                BinaryOperator::Plus,
                Box::new(Expr::Literal(1, Literal::String(String::from("hi")))),
                Box::new(Expr::Unary(
                    1,
                    UnaryOperator::Minus,
                    Box::new(Expr::Literal(1, Literal::Identifier(String::from("x")))),
                )),
            )),
        );

        assert_eq!(
            expr.to_string(),
            "(<=, (+, \"hi\", (-, x)), (+, \"hi\", (-, x)))"
        );
    }

    #[test]
    fn parses_expressions_correct_precendence() {
        // 4
        let tokens = vec![Token::new(TokenKind::Number(4.0), 1)];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Literal(1, Literal::Number(4.0)))
        );

        // - 4
        let tokens = vec![
            Token::new(TokenKind::Minus, 1),
            Token::new(TokenKind::Number(4.0), 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Unary(
                1,
                UnaryOperator::Minus,
                Box::new(Expr::Literal(1, Literal::Number(4.0)))
            ))
        );

        //ignore !!true
        let tokens = vec![
            Token::new(TokenKind::Bang, 1),
            Token::new(TokenKind::Bang, 1),
            Token::new(TokenKind::True, 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Unary(
                1,
                UnaryOperator::Bang,
                Box::new(Expr::Unary(
                    1,
                    UnaryOperator::Bang,
                    Box::new(Expr::Literal(1, Literal::True))
                ))
            ))
        );

        // 4 * 3
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Star, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Binary(
                1,
                BinaryOperator::Star,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            ))
        );

        // 4 + 3
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Plus, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Binary(
                1,
                BinaryOperator::Plus,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            ))
        );

        // 4 > 3
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Greater, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Binary(
                1,
                BinaryOperator::Greater,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            ))
        );

        // 4 == 3
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::EqualEqual, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Binary(
                1,
                BinaryOperator::EqualEqual,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            ))
        );

        // 4 * (3 + 2)
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Star, 1),
            Token::new(TokenKind::Number(3.0), 1),
            Token::new(TokenKind::LeftParen, 1),
            Token::new(TokenKind::Number(3.0), 1),
            Token::new(TokenKind::Plus, 1),
            Token::new(TokenKind::Number(2.0), 1),
            Token::new(TokenKind::RightParen, 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Binary(
                1,
                BinaryOperator::Star,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0)))
            ))
        );

        // 4, 3, 4 == 3
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Comma, 1),
            Token::new(TokenKind::Number(3.0), 1),
            Token::new(TokenKind::Comma, 1),
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::EqualEqual, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Binary(
                1,
                BinaryOperator::Comma,
                Box::new(Expr::Binary(
                    1,
                    BinaryOperator::Comma,
                    Box::new(Expr::Literal(1, Literal::Number(4.0))),
                    Box::new(Expr::Literal(1, Literal::Number(3.0))),
                )),
                Box::new(Expr::Binary(
                    1,
                    BinaryOperator::EqualEqual,
                    Box::new(Expr::Literal(1, Literal::Number(4.0))),
                    Box::new(Expr::Literal(1, Literal::Number(3.0))),
                ))
            ))
        );

        // 4 * 3 - 2 > -13 == true, false
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Star, 1),
            Token::new(TokenKind::Number(3.0), 1),
            Token::new(TokenKind::Minus, 1),
            Token::new(TokenKind::Number(2.0), 1),
            Token::new(TokenKind::Greater, 1),
            Token::new(TokenKind::Minus, 1),
            Token::new(TokenKind::Number(13.0), 1),
            Token::new(TokenKind::Comma, 1),
            Token::new(TokenKind::False, 1),
        ];

        assert_eq!(
            parse_expression(&mut tokens.iter().enumerate().peekable()),
            Ok(Expr::Binary(
                1,
                BinaryOperator::Comma,
                Box::new(Expr::Binary(
                    1,
                    BinaryOperator::Greater,
                    Box::new(Expr::Binary(
                        1,
                        BinaryOperator::Minus,
                        Box::new(Expr::Binary(
                            1,
                            BinaryOperator::Star,
                            Box::new(Expr::Literal(1, Literal::Number(4.0))),
                            Box::new(Expr::Literal(1, Literal::Number(3.0))),
                        )),
                        Box::new(Expr::Literal(1, Literal::Number(2.0))),
                    )),
                    Box::new(Expr::Unary(
                        1,
                        UnaryOperator::Minus,
                        Box::new(Expr::Literal(1, Literal::Number(13.0)))
                    ))
                )),
                Box::new(Expr::Literal(1, Literal::False))
            )),
        );

        // + 3
        let tokens = vec![
            Token::new(TokenKind::Plus, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse(&tokens),
            Err(SyntaxError::new(String::from("Expected an expression"), 1))
        );

        // (3 + 2
        let tokens = vec![
            Token::new(TokenKind::LeftParen, 1),
            Token::new(TokenKind::Number(3.0), 1),
            Token::new(TokenKind::Plus, 1),
            Token::new(TokenKind::Number(2.0), 1),
        ];

        assert_eq!(
            parse(&tokens),
            Err(SyntaxError::new(
                String::from("Expected a closing parenthese"),
                1
            ))
        );

        // 4 - ()
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Minus, 1),
            Token::new(TokenKind::LeftParen, 1),
            Token::new(TokenKind::RightParen, 1),
        ];
        assert_eq!(
            parse(&tokens),
            Err(SyntaxError::new(String::from("Expected an expression"), 1))
        );

        // .3 - 2
        let tokens = vec![
            Token::new(TokenKind::Dot, 1),
            Token::new(TokenKind::Number(3.0), 1),
            Token::new(TokenKind::Minus, 1),
            Token::new(TokenKind::Number(2.0), 1),
        ];
        assert_eq!(
            parse(&tokens),
            Err(SyntaxError::new(String::from("Expected an expression"), 1))
        );
    }

    #[test]
    fn parses_statements() {
        // 4 * 3;
        let tokens = [
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Star, 1),
            Token::new(TokenKind::Number(3.0), 1),
            Token::new(TokenKind::Semicolon, 1),
        ];

        assert_eq!(
            parse(&tokens),
            Ok(vec![Statement::Expr(Expr::Binary(
                1,
                BinaryOperator::Star,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            ))])
        )
    }

    #[test]
    fn unary_expressions_eval() {
        // -4
        assert_eq!(
            Expr::Unary(
                1,
                UnaryOperator::Minus,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
            )
            .eval(),
            Ok(Literal::Number(-4.0))
        );

        // -"yosef"
        assert_eq!(
            Expr::Unary(
                1,
                UnaryOperator::Minus,
                Box::new(Expr::Literal(1, Literal::String(String::from("yosef")))),
            )
            .eval(),
            Err(RuntimeError::new(
                String::from("The negative operator works only with numbers"),
                1
            ))
        );

        //ignore !true
        assert_eq!(
            Expr::Unary(
                1,
                UnaryOperator::Bang,
                Box::new(Expr::Literal(1, Literal::True))
            )
            .eval(),
            Ok(Literal::False)
        );

        //ignore !nil
        assert_eq!(
            Expr::Unary(
                1,
                UnaryOperator::Bang,
                Box::new(Expr::Literal(1, Literal::Nil))
            )
            .eval(),
            Ok(Literal::True)
        );

        //ignore !nil
        assert_eq!(
            Expr::Unary(
                1,
                UnaryOperator::Bang,
                Box::new(Expr::Literal(1, Literal::Nil))
            )
            .eval(),
            Ok(Literal::True)
        );

        //ignore !"nil"
        assert_eq!(
            Expr::Unary(
                1,
                UnaryOperator::Bang,
                Box::new(Expr::Literal(1, Literal::String(String::from("nil"))))
            )
            .eval(),
            Ok(Literal::False)
        );

        //ignore !""
        assert_eq!(
            Expr::Unary(
                1,
                UnaryOperator::Bang,
                Box::new(Expr::Literal(1, Literal::String(String::from(""))))
            )
            .eval(),
            Ok(Literal::False)
        );

        //ignore !0
        assert_eq!(
            Expr::Unary(
                1,
                UnaryOperator::Bang,
                Box::new(Expr::Literal(1, Literal::Number(0.0)))
            )
            .eval(),
            Ok(Literal::False)
        );
    }

    #[test]
    fn binary_expressions_eval() {
        // 4 + 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Plus,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::Number(7.0)),
        );

        // "yosef" + "mostafa"
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Plus,
                Box::new(Expr::Literal(1, Literal::String(String::from("yosef")))),
                Box::new(Expr::Literal(1, Literal::String(String::from("mostafa")))),
            )
            .eval(),
            Ok(Literal::String(String::from("yosefmostafa"))),
        );

        // "yosef" + 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Plus,
                Box::new(Expr::Literal(1, Literal::String(String::from("yosef")))),
                Box::new(Expr::Literal(1, Literal::Number(3.0)))
            )
            .eval(),
            Err(RuntimeError::new(
                String::from("Both operands should be either strings or numbers"),
                1
            ))
        );

        // 4 - 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Minus,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::Number(1.0)),
        );

        // 4 * 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Star,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::Number(12.0)),
        );

        // 4 * "yosef"
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Star,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::String(String::from("yosef")))),
            )
            .eval(),
            Err(RuntimeError::new(
                String::from("Both operands should be numbers"),
                1
            )),
        );

        // 4 / 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Slash,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            // 😃
            Ok(Literal::Number(4.0 / 3.0)),
        );

        // 4 != 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::BangEqual,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::True),
        );

        // 4 == "4"
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::EqualEqual,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::String(String::from("4")))),
            )
            .eval(),
            Ok(Literal::False)
        );

        // 4 == 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::EqualEqual,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::False),
        );

        // 4 > 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Greater,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::True),
        );

        // 4 >= 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::GreaterEqual,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::True),
        );

        // 4 < 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Less,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::False),
        );

        // 4 <= 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::LessEqual,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::False),
        );

        // 4 <= "3"
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::LessEqual,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::String(String::from("3")))),
            )
            .eval(),
            Err(RuntimeError::new(
                String::from("Both operands should be numbers"),
                1
            )),
        );

        // 4, 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Comma,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Literal(1, Literal::Number(3.0))),
            )
            .eval(),
            Ok(Literal::Number(3.0)),
        );

        // 4, 3 == 3
        assert_eq!(
            Expr::Binary(
                1,
                BinaryOperator::Comma,
                Box::new(Expr::Literal(1, Literal::Number(4.0))),
                Box::new(Expr::Binary(
                    1,
                    BinaryOperator::EqualEqual,
                    Box::new(Expr::Literal(1, Literal::Number(3.0))),
                    Box::new(Expr::Literal(1, Literal::Number(3.0))),
                )),
            )
            .eval(),
            Ok(Literal::True),
        );
    }
}
