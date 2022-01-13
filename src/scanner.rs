use super::SyntaxError;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // One character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    Semicolon,

    // One or more character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    String(String),
    Number(f64),
    Identifier(String),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    End,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize) -> Self {
        Self { kind, line }
    }
}

pub fn scan(code: &str) -> Result<Vec<Token>, SyntaxError> {
    let keywords_map = HashMap::from([
        (String::from("and"), TokenKind::And),
        (String::from("class"), TokenKind::Class),
        (String::from("else"), TokenKind::Else),
        (String::from("false"), TokenKind::False),
        (String::from("fun"), TokenKind::Fun),
        (String::from("for"), TokenKind::For),
        (String::from("if"), TokenKind::If),
        (String::from("nil"), TokenKind::Nil),
        (String::from("or"), TokenKind::Or),
        (String::from("print"), TokenKind::Print),
        (String::from("return"), TokenKind::Return),
        (String::from("super"), TokenKind::Super),
        (String::from("this"), TokenKind::This),
        (String::from("true"), TokenKind::True),
        (String::from("var"), TokenKind::Var),
        (String::from("while"), TokenKind::While),
    ]);

    let mut tokens = vec![];
    let mut chs = code.chars().peekable();
    let mut line = 1;

    while let Some(ch) = chs.next() {
        // whitespaces
        if ch == ' ' || ch == '\t' || ch == '\r' {
            continue;
        }
        if ch == '\n' {
            line += 1;
            continue;
        }

        // One character tokens
        if ch == '(' {
            tokens.push(Token::new(TokenKind::LeftParen, line));
            continue;
        }
        if ch == ')' {
            tokens.push(Token::new(TokenKind::RightParen, line));
            continue;
        }
        if ch == '{' {
            tokens.push(Token::new(TokenKind::LeftBrace, line));
            continue;
        }
        if ch == '}' {
            tokens.push(Token::new(TokenKind::RightBrace, line));
            continue;
        }
        if ch == ',' {
            tokens.push(Token::new(TokenKind::Comma, line));
            continue;
        }
        if ch == '.' {
            tokens.push(Token::new(TokenKind::Dot, line));
            continue;
        }
        if ch == '+' {
            tokens.push(Token::new(TokenKind::Plus, line));
            continue;
        }
        if ch == '-' {
            tokens.push(Token::new(TokenKind::Minus, line));
            continue;
        }
        if ch == '*' {
            tokens.push(Token::new(TokenKind::Star, line));
            continue;
        }
        if ch == ';' {
            tokens.push(Token::new(TokenKind::Semicolon, line));
            continue;
        }

        // One character tokens > '/'
        if ch == '/' {
            // One-line comments
            if let Some('/') = chs.peek() {
                loop {
                    if let Some(ch) = chs.next() {
                        if ch == '\n' {
                            line += 1;
                            break;
                        } else {
                            continue;
                        }
                    } else {
                        break;
                    }
                }
                continue;
            }

            // Multi-line comments
            if let Some('*') = chs.peek() {
                loop {
                    if let Some(ch) = chs.next() {
                        if ch == '\n' {
                            line += 1;
                        }

                        if ch == '*' {
                            if let Some('/') = chs.peek() {
                                chs.next();
                                break;
                            }
                        }
                        continue;
                    } else {
                        return Err(SyntaxError::new(
                            String::from("Unterminated multi-line comment"),
                            line,
                        ));
                    }
                }
                continue;
            }

            tokens.push(Token::new(TokenKind::Slash, line));
            continue;
        }

        // One or more character tokens
        if ch == '!' {
            if let Some('=') = chs.peek() {
                chs.next();
                tokens.push(Token::new(TokenKind::BangEqual, line));
                continue;
            }

            tokens.push(Token::new(TokenKind::Bang, line));
            continue;
        }

        if ch == '=' {
            if let Some('=') = chs.peek() {
                chs.next();
                tokens.push(Token::new(TokenKind::EqualEqual, line));
                continue;
            }

            tokens.push(Token::new(TokenKind::Equal, line));
            continue;
        }

        if ch == '>' {
            if let Some('=') = chs.peek() {
                chs.next();
                tokens.push(Token::new(TokenKind::GreaterEqual, line));
                continue;
            }

            tokens.push(Token::new(TokenKind::Greater, line));
            continue;
        }

        if ch == '<' {
            if let Some('=') = chs.peek() {
                chs.next();
                tokens.push(Token::new(TokenKind::LessEqual, line));
                continue;
            }

            tokens.push(Token::new(TokenKind::Less, line));
            continue;
        }

        // String literals
        if ch == '"' {
            let mut value = String::new();
            loop {
                match chs.peek() {
                    Some(ch) => {
                        if *ch == '"' {
                            chs.next();
                            tokens.push(Token::new(TokenKind::String(value), line));
                            break;
                        }

                        if *ch == '\n' {
                            line += 1;
                        }

                        value.push(*ch);
                        chs.next();
                        continue;
                    }
                    None => {
                        return Err(SyntaxError::new(String::from("Unterminated string"), line))
                    }
                }
            }
            continue;
        }

        /*
            * Numbers
                - Allowed syntax: 44 44.0 44.12433
                - Disalloed sytnax: .4 4.
        */

        if ch.is_ascii_digit() {
            let mut value = String::from(ch);

            // match the first part (the one before '.')
            while let Some(ch) = chs.peek() {
                if ch.is_ascii_digit() {
                    value.push(*ch);
                    chs.next();
                    continue;
                }
                break;
            }

            if let Some('.') = chs.peek() {
                chs.next();

                if let Some(ch) = chs.peek() {
                    // match the second part
                    if ch.is_ascii_digit() {
                        value.push('.');
                        while let Some(ch) = chs.peek() {
                            if ch.is_ascii_digit() {
                                value.push(*ch);
                                chs.next();
                                continue;
                            }
                            break;
                        }
                    } else {
                        tokens.push(Token::new(TokenKind::Number(value.parse().unwrap()), line));
                        tokens.push(Token::new(TokenKind::Dot, line));
                        continue;
                    }
                } else {
                    tokens.push(Token::new(TokenKind::Number(value.parse().unwrap()), line));
                    tokens.push(Token::new(TokenKind::Dot, line));
                    continue;
                }
            }

            tokens.push(Token::new(TokenKind::Number(value.parse().unwrap()), line));
            continue;
        }

        // Identifiers and keywords
        if ch.is_ascii_alphabetic() || ch == '_' {
            let mut value = String::from(ch);

            while let Some(ch) = chs.peek() {
                if ch.is_ascii_alphabetic() || *ch == '_' {
                    value.push(*ch);
                    chs.next();
                } else {
                    break;
                }
            }

            match keywords_map.get(&value) {
                Some(keyword) => {
                    tokens.push(Token::new(keyword.clone(), line));
                }
                None => tokens.push(Token::new(TokenKind::Identifier(value), line)),
            }
            continue;
        }

        return Err(SyntaxError::new(
            format!("Unexpected character {}", ch),
            line,
        ));
    }

    tokens.push(Token::new(TokenKind::End, line));

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scans_one_character_tokens() {
        assert_eq!(
            scan("(){},.+-*;/"),
            Ok(vec![
                Token::new(TokenKind::LeftParen, 1),
                Token::new(TokenKind::RightParen, 1),
                Token::new(TokenKind::LeftBrace, 1),
                Token::new(TokenKind::RightBrace, 1),
                Token::new(TokenKind::Comma, 1),
                Token::new(TokenKind::Dot, 1),
                Token::new(TokenKind::Plus, 1),
                Token::new(TokenKind::Minus, 1),
                Token::new(TokenKind::Star, 1),
                Token::new(TokenKind::Semicolon, 1),
                Token::new(TokenKind::Slash, 1),
                Token::new(TokenKind::End, 1),
            ])
        )
    }

    #[test]
    fn skips_comments() {
        assert_eq!(scan("// hi"), Ok(vec![Token::new(TokenKind::End, 1)]));
        assert_eq!(
            scan("// hi\n+"),
            Ok(vec![
                Token::new(TokenKind::Plus, 2),
                Token::new(TokenKind::End, 2)
            ])
        );
        assert_eq!(
            scan("/* hi */\n+"),
            Ok(vec![
                Token::new(TokenKind::Plus, 2),
                Token::new(TokenKind::End, 2)
            ])
        );
        assert_eq!(
            scan("/* hi\n+"),
            Err(SyntaxError::new(
                String::from("Unterminated multi-line comment"),
                2
            ))
        );
    }

    #[test]
    fn scans_one_or_more_character_tokens() {
        assert_eq!(
            scan("! != = == > >= < <="),
            Ok(vec![
                Token::new(TokenKind::Bang, 1),
                Token::new(TokenKind::BangEqual, 1),
                Token::new(TokenKind::Equal, 1),
                Token::new(TokenKind::EqualEqual, 1),
                Token::new(TokenKind::Greater, 1),
                Token::new(TokenKind::GreaterEqual, 1),
                Token::new(TokenKind::Less, 1),
                Token::new(TokenKind::LessEqual, 1),
                Token::new(TokenKind::End, 1),
            ])
        )
    }

    #[test]
    fn throws_unexpected_character_errors() {
        assert_eq!(
            scan("#"),
            Err(SyntaxError::new(String::from("Unexpected character #"), 1))
        )
    }

    #[test]
    fn scans_string_literals() {
        assert_eq!(
            scan("\"How are you my friend\""),
            Ok(vec![
                Token::new(TokenKind::String(String::from("How are you my friend")), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("\"How are\n you my friend\""),
            Ok(vec![
                Token::new(
                    TokenKind::String(String::from("How are\n you my friend")),
                    2
                ),
                Token::new(TokenKind::End, 2)
            ])
        );
        assert_eq!(
            scan("\"How are you my friend\n\""),
            Ok(vec![
                Token::new(
                    TokenKind::String(String::from("How are you my friend\n")),
                    2
                ),
                Token::new(TokenKind::End, 2)
            ])
        );
    }

    #[test]
    fn scans_numbers() {
        assert_eq!(
            scan("42"),
            Ok(vec![
                Token::new(TokenKind::Number(42.0), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("42.0"),
            Ok(vec![
                Token::new(TokenKind::Number(42.0), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("42.25"),
            Ok(vec![
                Token::new(TokenKind::Number(42.25), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("42."),
            Ok(vec![
                Token::new(TokenKind::Number(42.0), 1),
                Token::new(TokenKind::Dot, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan(".42"),
            Ok(vec![
                Token::new(TokenKind::Dot, 1),
                Token::new(TokenKind::Number(42.0), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
    }

    #[test]
    fn scans_identifiers() {
        assert_eq!(
            scan("oranges"),
            Ok(vec![
                Token::new(TokenKind::Identifier(String::from("oranges")), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("apples"),
            Ok(vec![
                Token::new(TokenKind::Identifier(String::from("apples")), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("nile"),
            Ok(vec![
                Token::new(TokenKind::Identifier(String::from("nile")), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("falsely"),
            Ok(vec![
                Token::new(TokenKind::Identifier(String::from("falsely")), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("_i"),
            Ok(vec![
                Token::new(TokenKind::Identifier(String::from("_i")), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("strong_arms"),
            Ok(vec![
                Token::new(TokenKind::Identifier(String::from("strong_arms")), 1),
                Token::new(TokenKind::End, 1)
            ])
        );
    }

    #[test]
    fn scans_keywords() {
        assert_eq!(
            scan("and"),
            Ok(vec![
                Token::new(TokenKind::And, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("class"),
            Ok(vec![
                Token::new(TokenKind::Class, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("else"),
            Ok(vec![
                Token::new(TokenKind::Else, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("false"),
            Ok(vec![
                Token::new(TokenKind::False, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("fun"),
            Ok(vec![
                Token::new(TokenKind::Fun, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("for"),
            Ok(vec![
                Token::new(TokenKind::For, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("if"),
            Ok(vec![
                Token::new(TokenKind::If, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("nil"),
            Ok(vec![
                Token::new(TokenKind::Nil, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("or"),
            Ok(vec![
                Token::new(TokenKind::Or, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("print"),
            Ok(vec![
                Token::new(TokenKind::Print, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("return"),
            Ok(vec![
                Token::new(TokenKind::Return, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("super"),
            Ok(vec![
                Token::new(TokenKind::Super, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("this"),
            Ok(vec![
                Token::new(TokenKind::This, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("true"),
            Ok(vec![
                Token::new(TokenKind::True, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("var"),
            Ok(vec![
                Token::new(TokenKind::Var, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
        assert_eq!(
            scan("while"),
            Ok(vec![
                Token::new(TokenKind::While, 1),
                Token::new(TokenKind::End, 1)
            ])
        );
    }
}
