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
}
