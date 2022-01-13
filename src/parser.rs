use super::scanner::Token;
use super::scanner::TokenKind;
use super::SyntaxError;
use std::convert::TryFrom;
use std::iter::Peekable;
use std::slice::Iter;

/*
    GRAMMAR
        expression -> comma
        comma -> equality ("," equality)*
        equality -> comparison (("==" | "!=") comparison)*
        comparison -> term ((">" | ">=" | "<" | "<=") term)*
        term -> factor (("+" | "-") factor)*
        factor -> unary (("*" | "/") unary)*
        unary -> ("-" | "!") unary | primary
        primary -> STRING | NUMBER | IDENTIFIER | TRUE | FALSE | NIL | "(" expression ")"

*/

#[derive(PartialEq, Debug)]
enum Literal {
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
enum UnaryOperator {
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
enum BinaryOperator {
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
    Literal(Literal),
    Unary(UnaryOperator, Box<Expr>),
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
    Grouping(Box<Expr>),
}

impl Expr {
    fn to_string(&self) -> String {
        match self {
            Self::Literal(literal) => literal.to_string(),
            Self::Binary(operator, expr_1, expr_2) => {
                format!(
                    "({}, {}, {})",
                    operator.to_string(),
                    expr_1.to_string(),
                    expr_2.to_string(),
                )
            }
            Self::Unary(operator, expr) => {
                format!("({}, {})", operator.to_string(), expr.to_string())
            }
            Self::Grouping(expr) => {
                format!("(group {})", expr.to_string())
            }
        }
    }
}

fn parse_primary(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, SyntaxError> {
    let token = tokens_iter.next().unwrap();

    if let Ok(literal) = Literal::try_from(token.kind.clone()) {
        Ok(Expr::Literal(literal))
    } else {
        match token.kind {
            TokenKind::LeftParen => {
                let expression = parse_expression(tokens_iter)?;

                let err =
                    SyntaxError::new(String::from("Expected a closing parenthese"), token.line);

                if let Some(token) = tokens_iter.next() {
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

fn parse_unary(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, SyntaxError> {
    let token = tokens_iter.peek().unwrap();

    if let Ok(unary_operator) = UnaryOperator::try_from(token.kind.clone()) {
        tokens_iter.next();
        Ok(Expr::Unary(
            unary_operator,
            Box::new(parse_unary(tokens_iter)?),
        ))
    } else {
        Ok(parse_primary(tokens_iter)?)
    }
}

fn parse_factor(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, SyntaxError> {
    let mut factor = parse_unary(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::Star => {
                    tokens_iter.next();
                    let unary = parse_unary(tokens_iter)?;

                    factor = Expr::Binary(binary_operator, Box::new(factor), Box::new(unary));
                }
                BinaryOperator::Slash => {
                    tokens_iter.next();
                    let unary = parse_unary(tokens_iter)?;

                    factor = Expr::Binary(binary_operator, Box::new(factor), Box::new(unary));
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

fn parse_term(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, SyntaxError> {
    let mut term = parse_factor(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::Plus => {
                    tokens_iter.next();
                    let factor = parse_factor(tokens_iter)?;

                    term = Expr::Binary(binary_operator, Box::new(term), Box::new(factor));
                }
                BinaryOperator::Minus => {
                    tokens_iter.next();
                    let factor = parse_factor(tokens_iter)?;

                    term = Expr::Binary(binary_operator, Box::new(term), Box::new(factor));
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

fn parse_comparison(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, SyntaxError> {
    let mut comparison = parse_term(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::Greater => {
                    tokens_iter.next();
                    let term = parse_term(tokens_iter)?;

                    comparison =
                        Expr::Binary(binary_operator, Box::new(comparison), Box::new(term));
                }
                BinaryOperator::GreaterEqual => {
                    tokens_iter.next();
                    let term = parse_term(tokens_iter)?;

                    comparison =
                        Expr::Binary(binary_operator, Box::new(comparison), Box::new(term));
                }
                BinaryOperator::Less => {
                    tokens_iter.next();
                    let term = parse_term(tokens_iter)?;

                    comparison =
                        Expr::Binary(binary_operator, Box::new(comparison), Box::new(term));
                }
                BinaryOperator::LessEqual => {
                    tokens_iter.next();
                    let term = parse_term(tokens_iter)?;

                    comparison =
                        Expr::Binary(binary_operator, Box::new(comparison), Box::new(term));
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

fn parse_equality(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, SyntaxError> {
    let mut equality = parse_comparison(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::BangEqual => {
                    tokens_iter.next();
                    let comparison = parse_comparison(tokens_iter)?;

                    equality =
                        Expr::Binary(binary_operator, Box::new(equality), Box::new(comparison));
                }
                BinaryOperator::EqualEqual => {
                    tokens_iter.next();
                    let comparison = parse_comparison(tokens_iter)?;

                    equality =
                        Expr::Binary(binary_operator, Box::new(equality), Box::new(comparison));
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

fn parse_comma(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, SyntaxError> {
    let mut comma = parse_equality(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.kind.clone()) {
            match binary_operator {
                BinaryOperator::Comma => {
                    tokens_iter.next();
                    let equality = parse_equality(tokens_iter)?;

                    comma = Expr::Binary(binary_operator, Box::new(comma), Box::new(equality));
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

fn parse_expression(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, SyntaxError> {
    let comma = parse_comma(tokens_iter)?;

    Ok(comma)
}

pub fn parse(tokens: &[Token]) -> Result<Expr, SyntaxError> {
    let mut tokens_iter = tokens.iter().peekable();
    let expression = parse_expression(&mut tokens_iter)?;
    Ok(expression)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_to_string() {
        let expr = Expr::Binary(
            BinaryOperator::LessEqual,
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                BinaryOperator::Minus,
                Box::new(Expr::Literal(Literal::Number(3.0))),
                Box::new(Expr::Literal(Literal::Number(2.0))),
            )))),
            Box::new(Expr::Binary(
                BinaryOperator::Plus,
                Box::new(Expr::Literal(Literal::String(String::from("hi")))),
                Box::new(Expr::Unary(
                    UnaryOperator::Minus,
                    Box::new(Expr::Literal(Literal::Identifier(String::from("x")))),
                )),
            )),
        );

        assert_eq!(
            expr.to_string(),
            "(<=, (group (-, 3, 2)), (+, \"hi\", (-, x)))"
        );
    }

    #[test]
    fn parses_with_correct_precendence() {
        // 4
        let tokens = vec![Token::new(TokenKind::Number(4.0), 1)];

        assert_eq!(parse(&tokens), Ok(Expr::Literal(Literal::Number(4.0))));

        // - 4
        let tokens = vec![
            Token::new(TokenKind::Minus, 1),
            Token::new(TokenKind::Number(4.0), 1),
        ];

        assert_eq!(
            parse(&tokens),
            Ok(Expr::Unary(
                UnaryOperator::Minus,
                Box::new(Expr::Literal(Literal::Number(4.0)))
            ))
        );

        //ignore !!true
        let tokens = vec![
            Token::new(TokenKind::Bang, 1),
            Token::new(TokenKind::Bang, 1),
            Token::new(TokenKind::True, 1),
        ];

        assert_eq!(
            parse(&tokens),
            Ok(Expr::Unary(
                UnaryOperator::Bang,
                Box::new(Expr::Unary(
                    UnaryOperator::Bang,
                    Box::new(Expr::Literal(Literal::True))
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
            parse(&tokens),
            Ok(Expr::Binary(
                BinaryOperator::Star,
                Box::new(Expr::Literal(Literal::Number(4.0))),
                Box::new(Expr::Literal(Literal::Number(3.0))),
            ))
        );

        // 4 + 3
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Plus, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse(&tokens),
            Ok(Expr::Binary(
                BinaryOperator::Plus,
                Box::new(Expr::Literal(Literal::Number(4.0))),
                Box::new(Expr::Literal(Literal::Number(3.0))),
            ))
        );

        // 4 > 3
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::Greater, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse(&tokens),
            Ok(Expr::Binary(
                BinaryOperator::Greater,
                Box::new(Expr::Literal(Literal::Number(4.0))),
                Box::new(Expr::Literal(Literal::Number(3.0))),
            ))
        );

        // 4 == 3
        let tokens = vec![
            Token::new(TokenKind::Number(4.0), 1),
            Token::new(TokenKind::EqualEqual, 1),
            Token::new(TokenKind::Number(3.0), 1),
        ];

        assert_eq!(
            parse(&tokens),
            Ok(Expr::Binary(
                BinaryOperator::EqualEqual,
                Box::new(Expr::Literal(Literal::Number(4.0))),
                Box::new(Expr::Literal(Literal::Number(3.0))),
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
            parse(&tokens),
            Ok(Expr::Binary(
                BinaryOperator::Star,
                Box::new(Expr::Literal(Literal::Number(4.0))),
                Box::new(Expr::Literal(Literal::Number(3.0)))
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
            parse(&tokens),
            Ok(Expr::Binary(
                BinaryOperator::Comma,
                Box::new(Expr::Binary(
                    BinaryOperator::Comma,
                    Box::new(Expr::Literal(Literal::Number(4.0))),
                    Box::new(Expr::Literal(Literal::Number(3.0))),
                )),
                Box::new(Expr::Binary(
                    BinaryOperator::EqualEqual,
                    Box::new(Expr::Literal(Literal::Number(4.0))),
                    Box::new(Expr::Literal(Literal::Number(3.0))),
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
            parse(&tokens),
            Ok(Expr::Binary(
                BinaryOperator::Comma,
                Box::new(Expr::Binary(
                    BinaryOperator::Greater,
                    Box::new(Expr::Binary(
                        BinaryOperator::Minus,
                        Box::new(Expr::Binary(
                            BinaryOperator::Star,
                            Box::new(Expr::Literal(Literal::Number(4.0))),
                            Box::new(Expr::Literal(Literal::Number(3.0))),
                        )),
                        Box::new(Expr::Literal(Literal::Number(2.0))),
                    )),
                    Box::new(Expr::Unary(
                        UnaryOperator::Minus,
                        Box::new(Expr::Literal(Literal::Number(13.0)))
                    ))
                )),
                Box::new(Expr::Literal(Literal::False))
            )),
        );
    }

    #[test]
    fn throws_meaningful_errors() {
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
}
