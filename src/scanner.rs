use super::SyntaxError;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
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
    let keywords_map = HashMap::from([
        (String::from("and"), Token::And),
        (String::from("class"), Token::Class),
        (String::from("else"), Token::Else),
        (String::from("false"), Token::False),
        (String::from("fun"), Token::Fun),
        (String::from("for"), Token::For),
        (String::from("if"), Token::If),
        (String::from("nil"), Token::Nil),
        (String::from("or"), Token::Or),
        (String::from("print"), Token::Print),
        (String::from("return"), Token::Return),
        (String::from("super"), Token::Super),
        (String::from("this"), Token::This),
        (String::from("true"), Token::True),
        (String::from("var"), Token::Var),
        (String::from("while"), Token::While),
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
                    tokens.push(keyword.clone());
                }
                None => tokens.push(Token::Identifier(value)),
            }
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

    #[test]
    fn scans_identifiers() {
        assert_eq!(
            scan("oranges"),
            Ok(vec![Token::Identifier(String::from("oranges")), Token::End])
        );
        assert_eq!(
            scan("apples"),
            Ok(vec![Token::Identifier(String::from("apples")), Token::End])
        );
        assert_eq!(
            scan("nile"),
            Ok(vec![Token::Identifier(String::from("nile")), Token::End])
        );
        assert_eq!(
            scan("falsely"),
            Ok(vec![Token::Identifier(String::from("falsely")), Token::End])
        );
        assert_eq!(
            scan("_i"),
            Ok(vec![Token::Identifier(String::from("_i")), Token::End])
        );
        assert_eq!(
            scan("strong_arms"),
            Ok(vec![
                Token::Identifier(String::from("strong_arms")),
                Token::End
            ])
        );
    }

    #[test]
    fn scans_keywords() {
        assert_eq!(scan("and"), Ok(vec![Token::And, Token::End]));
        assert_eq!(scan("class"), Ok(vec![Token::Class, Token::End]));
        assert_eq!(scan("else"), Ok(vec![Token::Else, Token::End]));
        assert_eq!(scan("false"), Ok(vec![Token::False, Token::End]));
        assert_eq!(scan("fun"), Ok(vec![Token::Fun, Token::End]));
        assert_eq!(scan("for"), Ok(vec![Token::For, Token::End]));
        assert_eq!(scan("if"), Ok(vec![Token::If, Token::End]));
        assert_eq!(scan("nil"), Ok(vec![Token::Nil, Token::End]));
        assert_eq!(scan("or"), Ok(vec![Token::Or, Token::End]));
        assert_eq!(scan("print"), Ok(vec![Token::Print, Token::End]));
        assert_eq!(scan("return"), Ok(vec![Token::Return, Token::End]));
        assert_eq!(scan("super"), Ok(vec![Token::Super, Token::End]));
        assert_eq!(scan("this"), Ok(vec![Token::This, Token::End]));
        assert_eq!(scan("true"), Ok(vec![Token::True, Token::End]));
        assert_eq!(scan("var"), Ok(vec![Token::Var, Token::End]));
        assert_eq!(scan("while"), Ok(vec![Token::While, Token::End]));
    }
}
