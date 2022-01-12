use super::scanner::Token;
use std::convert::TryFrom;
use std::iter::Peekable;
use std::slice::Iter;

/*
    GRAMMAR
        expression -> equality
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

impl TryFrom<Token> for Literal {
    type Error = String;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::String(value) => Ok(Self::String(value)),
            Token::Number(value) => Ok(Self::Number(value)),
            Token::Identifier(value) => Ok(Self::Identifier(value)),
            Token::True => Ok(Self::True),
            Token::False => Ok(Self::False),
            Token::Nil => Ok(Self::Nil),
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

impl TryFrom<Token> for UnaryOperator {
    type Error = String;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Bang => Ok(Self::Bang),
            Token::Minus => Ok(Self::Minus),
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
        }
    }
}

impl TryFrom<Token> for BinaryOperator {
    type Error = String;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(Self::Plus),
            Token::Minus => Ok(Self::Minus),
            Token::Star => Ok(Self::Star),
            Token::Slash => Ok(Self::Slash),
            Token::BangEqual => Ok(Self::BangEqual),
            Token::EqualEqual => Ok(Self::EqualEqual),
            Token::Greater => Ok(Self::Greater),
            Token::GreaterEqual => Ok(Self::GreaterEqual),
            Token::Less => Ok(Self::Less),
            Token::LessEqual => Ok(Self::LessEqual),
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

fn expect(expected_token: Token, tokens_iter: &mut Peekable<Iter<Token>>) -> Result<(), String> {
    if let Some(token) = tokens_iter.next() {
        if expected_token == token.clone() {
            Ok(())
        } else {
            Err(format!(
                "Expected {:?} token, but got {:?} token",
                expected_token, token
            ))
        }
    } else {
        Err(format!(
            "Expected {:?} token, but got nothing",
            expected_token
        ))
    }
}

fn parse_primary(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
    let token = tokens_iter.next().unwrap();

    if let Ok(literal) = Literal::try_from(token.clone()) {
        Ok(Expr::Literal(literal))
    } else {
        match token {
            Token::LeftParen => {
               let expression = parse_expression(tokens_iter)?;
               expect(Token::RightParen, tokens_iter)?;
               Ok(expression)
            },
            token => Err(format!(
                "Expected a literal value or an expression wrapped inside parentheses, but got {:?} token", token
            )),
        }
    }
}

fn parse_unary(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
    let token = tokens_iter.peek().unwrap();

    if let Ok(unary_operator) = UnaryOperator::try_from(token.clone().clone()) {
        tokens_iter.next();
        Ok(Expr::Unary(
            unary_operator,
            Box::new(parse_unary(tokens_iter)?),
        ))
    } else {
        Ok(parse_primary(tokens_iter)?)
    }
}

fn parse_factor(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
    let mut factor = parse_unary(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.clone().clone()) {
            match binary_operator {
                BinaryOperator::Star => {
                    tokens_iter.next();
                    let factor_1 = parse_unary(tokens_iter)?;

                    factor = Expr::Binary(binary_operator, Box::new(factor), Box::new(factor_1));
                }
                BinaryOperator::Slash => {
                    tokens_iter.next();
                    let factor_1 = parse_unary(tokens_iter)?;

                    factor = Expr::Binary(binary_operator, Box::new(factor), Box::new(factor_1));
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

fn parse_term(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
    let mut term = parse_factor(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.clone().clone()) {
            match binary_operator {
                BinaryOperator::Plus => {
                    tokens_iter.next();
                    let term_1 = parse_factor(tokens_iter)?;

                    term = Expr::Binary(binary_operator, Box::new(term), Box::new(term_1));
                }
                BinaryOperator::Minus => {
                    tokens_iter.next();
                    let term_1 = parse_factor(tokens_iter)?;

                    term = Expr::Binary(binary_operator, Box::new(term), Box::new(term_1));
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

fn parse_comparison(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
    let mut comparison = parse_term(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.clone().clone()) {
            match binary_operator {
                BinaryOperator::Greater => {
                    tokens_iter.next();
                    let comparison_1 = parse_term(tokens_iter)?;

                    comparison = Expr::Binary(
                        binary_operator,
                        Box::new(comparison),
                        Box::new(comparison_1),
                    );
                }
                BinaryOperator::GreaterEqual => {
                    tokens_iter.next();
                    let comparison_1 = parse_term(tokens_iter)?;

                    comparison = Expr::Binary(
                        binary_operator,
                        Box::new(comparison),
                        Box::new(comparison_1),
                    );
                }
                BinaryOperator::Less => {
                    tokens_iter.next();
                    let comparison_1 = parse_term(tokens_iter)?;

                    comparison = Expr::Binary(
                        binary_operator,
                        Box::new(comparison),
                        Box::new(comparison_1),
                    );
                }
                BinaryOperator::LessEqual => {
                    tokens_iter.next();
                    let comparison_1 = parse_term(tokens_iter)?;

                    comparison = Expr::Binary(
                        binary_operator,
                        Box::new(comparison),
                        Box::new(comparison_1),
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

fn parse_equality(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
    let mut equality = parse_comparison(tokens_iter)?;

    while let Some(token) = tokens_iter.peek() {
        if let Ok(binary_operator) = BinaryOperator::try_from(token.clone().clone()) {
            match binary_operator {
                BinaryOperator::BangEqual => {
                    tokens_iter.next();
                    let equality_1 = parse_comparison(tokens_iter)?;

                    equality =
                        Expr::Binary(binary_operator, Box::new(equality), Box::new(equality_1));
                }
                BinaryOperator::EqualEqual => {
                    tokens_iter.next();
                    let equality_1 = parse_comparison(tokens_iter)?;

                    equality =
                        Expr::Binary(binary_operator, Box::new(equality), Box::new(equality_1));
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

fn parse_expression(tokens_iter: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
    let expression = parse_equality(tokens_iter)?;

    Ok(expression)
}

pub fn parse(tokens: &[Token]) -> Result<Expr, String> {
    let mut tokens_iter = tokens.iter().peekable();
    let expression = parse_expression(&mut tokens_iter)?;
    expect(Token::End, &mut tokens_iter)?;
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
        let tokens = vec![Token::Number(4.0), Token::End];

        assert_eq!(parse(&tokens), Ok(Expr::Literal(Literal::Number(4.0))));

        // - 4
        let tokens = vec![Token::Minus, Token::Number(4.0), Token::End];

        assert_eq!(
            parse(&tokens),
            Ok(Expr::Unary(
                UnaryOperator::Minus,
                Box::new(Expr::Literal(Literal::Number(4.0)))
            ))
        );

        //ignore !!true
        let tokens = vec![Token::Bang, Token::Bang, Token::True, Token::End];

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
            Token::Number(4.0),
            Token::Star,
            Token::Number(3.0),
            Token::End,
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
            Token::Number(4.0),
            Token::Plus,
            Token::Number(3.0),
            Token::End,
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
            Token::Number(4.0),
            Token::Greater,
            Token::Number(3.0),
            Token::End,
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
            Token::Number(4.0),
            Token::EqualEqual,
            Token::Number(3.0),
            Token::End,
        ];

        assert_eq!(
            parse(&tokens),
            Ok(Expr::Binary(
                BinaryOperator::EqualEqual,
                Box::new(Expr::Literal(Literal::Number(4.0))),
                Box::new(Expr::Literal(Literal::Number(3.0))),
            ))
        );
        // 4 * 3 - 2 > -13 == true
        let tokens = vec![
            Token::Number(4.0),
            Token::Star,
            Token::Number(3.0),
            Token::Minus,
            Token::Number(2.0),
            Token::Greater,
            Token::Minus,
            Token::Number(13.0),
            Token::End,
        ];

        assert_eq!(
            parse(&tokens),
            Ok(Expr::Binary(
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
        );
    }

    #[test]
    fn throws_meaningful_errors() {
        //TODO use SyntaxError
    }
}
