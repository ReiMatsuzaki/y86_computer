use super::token::Token;

pub fn tokenize(src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = src.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            ' ' | '\t' | '\n' => continue,
            '=' => {
                if let Some('=') = chars.peek() {
                    tokens.push(Token::Op2(['=', '=']));
                    chars.next();
                } else {
                    tokens.push(Token::Op('='));
                }
            }
            '+' | '-' | '*' | '(' | ')' | '<' | '>' | ';' | '{' | '}' | ',' | '&' | '[' | ']' => {
                tokens.push(Token::Op(c));
            }
            '/' => {
                if let Some('/') = chars.peek() {
                    while let Some(&c) = chars.peek() {
                        match c {
                            '\n' => break,
                            _ => {
                                chars.next();
                            }
                        }
                    }
                } else {
                    tokens.push(Token::Op('/'));
                }
            }
            '0'..='9' => {
                let mut num = String::new();
                num.push(c);
                while let Some(&c) = chars.peek() {
                    match c {
                        '0'..='9' => {
                            num.push(c);
                            chars.next();
                        }
                        _ => break,
                    }
                }
                tokens.push(Token::Num(num.parse::<u64>().unwrap()));
            }
            _ => {
                let mut buf = String::new();
                buf.push(c);
                while let Some(&c) = chars.peek() {
                    match c {
                        'a'..='z' | 'A'..='Z' | '0'..='9' => {
                            buf.push(c);
                            chars.next();
                        }
                        _ => break,
                    }
                }
                let t = match buf.as_str() {
                    "return" => Token::Return,
                    "if" => Token::If,
                    "while" => Token::While,
                    "int" => Token::Int,
                    _ => Token::Id(buf),
                };
                tokens.push(t);
            }
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let input = "13 + 2";
        let expe = vec![Token::Num(13), Token::Op('+'), Token::Num(2)];
        let calc = tokenize(input);
        assert_eq!(expe, calc);

        let input = "13 + 2==3";
        let expe = vec![
            Token::Num(13),
            Token::Op('+'),
            Token::Num(2),
            Token::Op2(['=', '=']),
            Token::Num(3),
        ];
        let calc = tokenize(input);
        assert_eq!(expe, calc);
    }

    #[test]
    fn test_statement() {
        let input = "while (a < 10) { if(a ==2) {int x = a + 1; return x; } }";
        let expe = vec![
            Token::While,
            Token::Op('('),
            Token::Id(String::from("a")),
            Token::Op('<'),
            Token::Num(10),
            Token::Op(')'),
            Token::Op('{'),
            Token::If,
            Token::Op('('),
            Token::Id(String::from("a")),
            Token::Op2(['=', '=']),
            Token::Num(2),
            Token::Op(')'),
            Token::Op('{'),
            Token::Int,
            Token::Id(String::from("x")),
            Token::Op('='),
            Token::Id(String::from("a")),
            Token::Op('+'),
            Token::Num(1),
            Token::Op(';'),
            Token::Return,
            Token::Id(String::from("x")),
            Token::Op(';'),
            Token::Op('}'),
            Token::Op('}'),
        ];
        let calc = tokenize(input);
        assert_eq!(expe, calc);
    }
}
