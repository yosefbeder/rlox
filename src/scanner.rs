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

        // One or more character tokens
        if ch == '!' {
            if let Some('=') = chs.peek() {
                chs.next();
                tokens.push(Token::BangEqual);
                continue;
            }

            tokens.push(Token::Bang);
            continue;
        }

        if ch == '=' {
            if let Some('=') = chs.peek() {
                chs.next();
                tokens.push(Token::EqualEqual);
                continue;
            }

            tokens.push(Token::Equal);
            continue;
        }

        if ch == '>' {
            if let Some('=') = chs.peek() {
                chs.next();
                tokens.push(Token::GreaterEqual);
                continue;
            }

            tokens.push(Token::Greater);
            continue;
        }

        if ch == '<' {
            if let Some('=') = chs.peek() {
                chs.next();
                tokens.push(Token::LessEqual);
                continue;
            }

            tokens.push(Token::Less);
            continue;
        }

        // String literals
        if ch == '"' {
            println!("Scanning a string literal {}", ch);

            let mut value = String::new();
            loop {
                match chs.peek() {
                    Some(ch) => {
                        if *ch == '"' {
                            chs.next();
                            tokens.push(Token::String(value));
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
                        tokens.push(Token::Number(value.parse().unwrap()));
                        tokens.push(Token::Dot);
                        continue;
                    }
                } else {
                    tokens.push(Token::Number(value.parse().unwrap()));
                    tokens.push(Token::Dot);
                    continue;
                }
            }

            tokens.push(Token::Number(value.parse().unwrap()));
            continue;
        }

        return Err(SyntaxError::new(
            format!("Unexpected character {}", ch),
            line,
        ));
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
    fn scans_one_or_more_character_tokens() {
        assert_eq!(
            scan("! != = == > >= < <="),
            Ok(vec![
                Token::Bang,
                Token::BangEqual,
                Token::Equal,
                Token::EqualEqual,
                Token::Greater,
                Token::GreaterEqual,
                Token::Less,
                Token::LessEqual,
                Token::End,
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
                Token::String(String::from("How are you my friend")),
                Token::End
            ])
        );
        assert_eq!(
            scan("\"How are\n you my friend\""),
            Ok(vec![
                Token::String(String::from("How are\n you my friend")),
                Token::End
            ])
        );
        assert_eq!(
            scan("\"How are you my friend\n\""),
            Ok(vec![
                Token::String(String::from("How are you my friend\n")),
                Token::End
            ])
        );
    }

    #[test]
    fn scans_numbers() {
        assert_eq!(scan("42"), Ok(vec![Token::Number(42.0), Token::End]));
        assert_eq!(scan("42.0"), Ok(vec![Token::Number(42.0), Token::End]));
        assert_eq!(scan("42.25"), Ok(vec![Token::Number(42.25), Token::End]));
        assert_eq!(
            scan("42."),
            Ok(vec![Token::Number(42.0), Token::Dot, Token::End])
        );
        assert_eq!(
            scan(".42"),
            Ok(vec![Token::Dot, Token::Number(42.0), Token::End])
        );
    }
}
