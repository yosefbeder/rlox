use super::SyntaxError;

#[derive(Debug, PartialEq)]
pub enum Token {
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
    Identifier,
    String,
    Number,

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

pub fn scan(code: &str) -> Result<Vec<Token>, SyntaxError> {
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
            tokens.push(Token::LeftParen);
            continue;
        }
        if ch == ')' {
            tokens.push(Token::RightParen);
            continue;
        }
        if ch == '{' {
            tokens.push(Token::LeftBrace);
            continue;
        }
        if ch == '}' {
            tokens.push(Token::RightBrace);
            continue;
        }
        if ch == ',' {
            tokens.push(Token::Comma);
            continue;
        }
        if ch == '.' {
            tokens.push(Token::Dot);
            continue;
        }
        if ch == '+' {
            tokens.push(Token::Plus);
            continue;
        }
        if ch == '-' {
            tokens.push(Token::Minus);
            continue;
        }
        if ch == '*' {
            tokens.push(Token::Star);
            continue;
        }
        if ch == ';' {
            tokens.push(Token::Semicolon);
            continue;
        }

        // One character tokens > '/' and tokens
        if ch == '/' {
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

            tokens.push(Token::Slash);
            continue;
        }
    }

    tokens.push(Token::End);

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
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::RightBrace,
                Token::Comma,
                Token::Dot,
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Semicolon,
                Token::Slash,
                Token::End,
            ])
        )
    }

    #[test]
    fn skips_comments() {
        assert_eq!(scan("// hi"), Ok(vec![Token::End]));
        assert_eq!(scan("// hi\n+"), Ok(vec![Token::Plus, Token::End]));
    }

    #[test]
    fn throws_unexpected_character_errors() {
        assert_eq!(
            scan("#"),
            Err(SyntaxError::new(String::from("Unexpected character #"), 1))
        )
    }
}
