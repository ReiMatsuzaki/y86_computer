use std::{iter::Peekable, str::Chars};

use super::token::Token;

struct Tokenizer<'a> {
    // tokens: Vec<Token>,
    line_num: usize,
    col_num: usize,
    chars: Peekable<Chars<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenInfo {
    pub line_num: usize,
    pub col_num: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(src: &str) -> Tokenizer {
        let chars = src.chars().peekable();
        Tokenizer {
            // tokens: Vec::new(),
            line_num: 1,
            col_num: 1,
            chars,
        }
    }

    fn next_with_info(&mut self) -> Option<(char, TokenInfo)> {
        let ti = TokenInfo {
            line_num: self.line_num,
            col_num: self.col_num,
        };
        self.next().map(|c| (c, ti))
    }

    fn next(&mut self) -> Option<char> {
        let c = self.chars.next();
        if let Some(c) = c {
            if c == '\n' {
                self.line_num += 1;
                self.col_num = 1;
            } else {
                self.col_num += 1;
            }
        }
        c
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    pub fn tokenize(&mut self) -> Vec<(Token, TokenInfo)> {
        let mut tokens = Vec::new();
        while let Some((c, ti)) = self.next_with_info() {
            match c {
                ' ' | '\t' | '\n' => continue,
                '=' => {
                    if let Some('=') = self.peek() {
                        tokens.push((Token::Op2(['=', '=']), ti));
                        self.next();
                    } else {
                        tokens.push((Token::Op('='), ti));
                    }
                }
                '+' | '-' | '*' | '(' | ')' | '<' | '>' | ';' | '{' | '}' | ',' | '&' | '[' | ']' => {
                    tokens.push((Token::Op(c), ti));
                }
                '/' => {
                    if let Some('/') = self.peek() {
                        while let Some(&c) = self.peek() {
                            match c {
                                '\n' => break,
                                _ => {
                                    self.next();
                                }
                            }
                        }
                    } else {
                        tokens.push((Token::Op('/'), ti));
                    }
                }
                '0'..='9' => {
                    if let Some('x') = self.peek() {
                        self.next();
                        let mut hex = String::new();
                        hex.push('0');
                        hex.push('x');
                        while let Some(&c) = self.peek() {
                            match c {
                                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                    hex.push(c);
                                    self.next();
                                }
                                _ => break,
                            }
                        }
                        let without_prefix = hex.trim_start_matches("0x");
                        let x = u64::from_str_radix(without_prefix, 16).unwrap();
                        tokens.push((Token::Hex(x), ti));
                    } else {
                        let mut num = String::new();
                        num.push(c);
                        while let Some(&c) = self.peek() {
                            match c {
                                '0'..='9' => {
                                    num.push(c);
                                    self.next();
                                }
                                _ => break,
                            }
                        }
                        tokens.push((Token::Num(num.parse::<u64>().unwrap()), ti));
                    }
                }
                '"' => {
                    let mut s = String::new();
                    while let Some(&c) = self.peek() {
                        match c {
                            '"' => {
                                self.next();
                                break;
                            }
                            _ => {
                                s.push(c);
                                self.next();
                            }
                        }
                    }
                    tokens.push((Token::Str(s), ti));
                }
                _ => {
                    let mut buf = String::new();
                    buf.push(c);
                    while let Some(&c) = self.peek() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' => {
                                buf.push(c);
                                self.next();
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
                    tokens.push((t, ti));
                }
            }
        }
        tokens
    }
}

pub fn tokenize(src: &str) -> (Vec<Token>, Vec<TokenInfo>) {
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize();
    let (tokens, token_infos): (Vec<_>, Vec<_>) = tokens.into_iter().unzip();
    (tokens, token_infos)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let input = "13 + 2";
        let expe = vec![Token::Num(13), Token::Op('+'), Token::Num(2)];
        let (calc, _) = tokenize(input);
        assert_eq!(expe, calc);

        let input = "13 + 2==3";
        let expe = vec![
            Token::Num(13),
            Token::Op('+'),
            Token::Num(2),
            Token::Op2(['=', '=']),
            Token::Num(3),
        ];
        let (calc, _) = tokenize(input);
        assert_eq!(expe, calc);
    }

    #[test]
    fn test_statement() {
        let input = "while (a < 10) { if(a ==0x1a) {int x = a + \"abc\"; return x; } }";
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
            Token::Hex(0x1A),
            Token::Op(')'),
            Token::Op('{'),
            Token::Int,
            Token::Id(String::from("x")),
            Token::Op('='),
            Token::Id(String::from("a")),
            Token::Op('+'),
            Token::Str("abc".to_string()),
            Token::Op(';'),
            Token::Return,
            Token::Id(String::from("x")),
            Token::Op(';'),
            Token::Op('}'),
            Token::Op('}'),
        ];
        let (calc, _) = tokenize(input);
        assert_eq!(expe, calc);
    }
}
