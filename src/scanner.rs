use super::errors::SyntaxError;
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

pub struct Scanner {
    code: String,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(code: String) -> Self {
        Self {
            code,
            current: 0,
            line: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        self.current += 1;
        self.code.chars().nth(self.current)
    }

    fn peek(&mut self) -> Option<char> {
        self.code.chars().nth(self.current + 1)
    }

    pub fn scan(&mut self) -> Result<Vec<Token>, SyntaxError> {
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

        while let Some(ch) = self.next() {
            // whitespaces
            if ch == ' ' || ch == '\t' || ch == '\r' {
                continue;
            }
            if ch == '\n' {
                self.line += 1;
                continue;
            }

            // One character tokens
            if ch == '(' {
                tokens.push(Token::new(TokenKind::LeftParen, self.line));
                continue;
            }
            if ch == ')' {
                tokens.push(Token::new(TokenKind::RightParen, self.line));
                continue;
            }
            if ch == '{' {
                tokens.push(Token::new(TokenKind::LeftBrace, self.line));
                continue;
            }
            if ch == '}' {
                tokens.push(Token::new(TokenKind::RightBrace, self.line));
                continue;
            }
            if ch == ',' {
                tokens.push(Token::new(TokenKind::Comma, self.line));
                continue;
            }
            if ch == '.' {
                tokens.push(Token::new(TokenKind::Dot, self.line));
                continue;
            }
            if ch == '+' {
                tokens.push(Token::new(TokenKind::Plus, self.line));
                continue;
            }
            if ch == '-' {
                tokens.push(Token::new(TokenKind::Minus, self.line));
                continue;
            }
            if ch == '*' {
                tokens.push(Token::new(TokenKind::Star, self.line));
                continue;
            }
            if ch == ';' {
                tokens.push(Token::new(TokenKind::Semicolon, self.line));
                continue;
            }

            // One character tokens > '/'
            if ch == '/' {
                // One-line comments
                if let Some('/') = self.peek() {
                    loop {
                        if let Some(ch) = self.next() {
                            if ch == '\n' {
                                self.line += 1;
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
                if let Some('*') = self.peek() {
                    loop {
                        if let Some(ch) = self.next() {
                            if ch == '\n' {
                                self.line += 1;
                            }

                            if ch == '*' {
                                if let Some('/') = self.peek() {
                                    self.next();
                                    break;
                                }
                            }
                            continue;
                        } else {
                            return Err(SyntaxError::new(
                                String::from("Unterminated multi-line comment"),
                                self.line,
                            ));
                        }
                    }
                    continue;
                }

                tokens.push(Token::new(TokenKind::Slash, self.line));
                continue;
            }

            // One or more character tokens
            if ch == '!' {
                if let Some('=') = self.peek() {
                    self.next();
                    tokens.push(Token::new(TokenKind::BangEqual, self.line));
                    continue;
                }

                tokens.push(Token::new(TokenKind::Bang, self.line));
                continue;
            }

            if ch == '=' {
                if let Some('=') = self.peek() {
                    self.next();
                    tokens.push(Token::new(TokenKind::EqualEqual, self.line));
                    continue;
                }

                tokens.push(Token::new(TokenKind::Equal, self.line));
                continue;
            }

            if ch == '>' {
                if let Some('=') = self.peek() {
                    self.next();
                    tokens.push(Token::new(TokenKind::GreaterEqual, self.line));
                    continue;
                }

                tokens.push(Token::new(TokenKind::Greater, self.line));
                continue;
            }

            if ch == '<' {
                if let Some('=') = self.peek() {
                    self.next();
                    tokens.push(Token::new(TokenKind::LessEqual, self.line));
                    continue;
                }

                tokens.push(Token::new(TokenKind::Less, self.line));
                continue;
            }

            // String literals
            if ch == '"' {
                let mut value = String::new();
                loop {
                    match self.peek() {
                        Some(ch) => {
                            if ch == '"' {
                                self.next();
                                tokens.push(Token::new(TokenKind::String(value), self.line));
                                break;
                            }

                            if ch == '\n' {
                                self.line += 1;
                            }

                            value.push(ch);
                            self.next();
                            continue;
                        }
                        None => {
                            return Err(SyntaxError::new(
                                String::from("Unterminated string"),
                                self.line,
                            ))
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
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_digit() {
                        value.push(ch);
                        self.next();
                        continue;
                    }
                    break;
                }

                if let Some('.') = self.peek() {
                    self.next();

                    if let Some(ch) = self.peek() {
                        // match the second part
                        if ch.is_ascii_digit() {
                            value.push('.');
                            while let Some(ch) = self.peek() {
                                if ch.is_ascii_digit() {
                                    value.push(ch);
                                    self.next();
                                    continue;
                                }
                                break;
                            }
                        } else {
                            tokens.push(Token::new(
                                TokenKind::Number(value.parse().unwrap()),
                                self.line,
                            ));
                            tokens.push(Token::new(TokenKind::Dot, self.line));
                            continue;
                        }
                    } else {
                        tokens.push(Token::new(
                            TokenKind::Number(value.parse().unwrap()),
                            self.line,
                        ));
                        tokens.push(Token::new(TokenKind::Dot, self.line));
                        continue;
                    }
                }

                tokens.push(Token::new(
                    TokenKind::Number(value.parse().unwrap()),
                    self.line,
                ));
                continue;
            }

            // Identifiers and keywords
            if ch.is_ascii_alphabetic() || ch == '_' {
                let mut value = String::from(ch);

                while let Some(ch) = self.peek() {
                    if ch.is_ascii_alphabetic() || ch == '_' {
                        value.push(ch);
                        self.next();
                    } else {
                        break;
                    }
                }

                match keywords_map.get(&value) {
                    Some(keyword) => {
                        tokens.push(Token::new(keyword.clone(), self.line));
                    }
                    None => tokens.push(Token::new(TokenKind::Identifier(value), self.line)),
                }
                continue;
            }

            return Err(SyntaxError::new(
                format!("Unexpected character {}", ch),
                self.line,
            ));
        }

        tokens.push(Token::new(TokenKind::End, self.line));

        Ok(tokens)
    }
}
