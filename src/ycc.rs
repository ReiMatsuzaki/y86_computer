/*  simple c compiler for Y86-64 */
use crate::yas::Statement;

pub mod tokenizer {
    #[derive(Debug, PartialEq)]
    pub enum Token {
        Op(char),
        Num(u64),
        Id(String),
    }

    pub fn tokenize(src: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = src.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                ' ' | '\t' | '\n' => continue,
                '+' | '-' | '*' | '/' | '(' | ')' => {
                    tokens.push(Token::Op(c));
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
                    let mut id = String::new();
                    id.push(c);
                    while let Some(&c) = chars.peek() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' => {
                                id.push(c);
                                chars.next();
                            }
                            _ => break,
                        }
                    }
                    tokens.push(Token::Id(id));
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
        }
    }    
}

pub mod parser {
    use super::tokenizer::Token;
    // BNF
    // expr    = mul ("+" mul | "-" mul)*
    // mul     = primary ("*" primary | "/" primary)*
    // primary = num | "(" expr ")"

    // context free grammer:
    // expr ::= mul | mul "+" expr | mul "-" expr
    #[derive(Debug, PartialEq)]
    pub enum Expr {
        Mul(Box<Mul>),
        Add(Box<Mul>, Box<Expr>),
        Sub(Box<Mul>, Box<Expr>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Mul {
        Primary(Box<Primary>),
        Mul(Box<Primary>, Box<Mul>),
        Div(Box<Primary>, Box<Mul>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Primary {
        Num(u64),
        Expr(Box<Expr>),
    }

    pub struct Parser {
        pub(crate) tokens: Vec<Token>,
        pos: usize,
    }

    impl Parser {
        pub fn new(tokens: Vec<Token>) -> Self {
            Parser { tokens, pos: 0 }
        }

        pub fn parse(&mut self) -> Expr {
            self.parse_expr()
        }

        fn parse_expr(&mut self) -> Expr {
            let mul = self.parse_mul();
            match self.tokens.get(self.pos) {
                Some(Token::Op('+')) => {
                    self.pos += 1;
                    Expr::Add(Box::new(mul), Box::new(self.parse_expr()))
                }
                Some(Token::Op('-')) => {
                    self.pos += 1;
                    Expr::Sub(Box::new(mul), Box::new(self.parse_expr()))
                }
                _ => Expr::Mul(Box::new(mul)),
            }
        }

        fn parse_mul(&mut self) -> Mul {
            let primary = self.parse_primary();
            match self.tokens.get(self.pos) {
                Some(Token::Op('*')) => {
                    self.pos += 1;
                    Mul::Mul(Box::new(primary), Box::new(self.parse_mul()))
                }
                Some(Token::Op('/')) => {
                    self.pos += 1;
                    Mul::Div(Box::new(primary), Box::new(self.parse_mul()))
                }
                _ => Mul::Primary(Box::new(primary)),
            }
        }

        fn parse_primary(&mut self) -> Primary {
            match self.tokens.get(self.pos) {
                Some(Token::Num(n)) => {
                    self.pos += 1;
                    Primary::Num(*n)
                }
                Some(Token::Op('(')) => {
                    self.pos += 1;
                    let expr = self.parse_expr();
                    match self.tokens.get(self.pos) {
                        Some(Token::Op(')')) => {
                            self.pos += 1;
                            Primary::Expr(Box::new(expr))
                        }
                        _ => panic!("expected token ')'. but found {:?}", self.tokens[self.pos]),
                    }
                }
                _ => panic!("unexpected token in primary: {:?}", self.tokens[self.pos]),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_expr() {
            let tokens = vec![
                Token::Num(1),
                Token::Op('+'),
                Token::Num(2),
                Token::Op('*'),
                Token::Num(3),
                Token::Op('-'),
                Token::Num(4),
                Token::Op('/'),
                Token::Num(5),
            ];
            let mut parser = Parser { tokens, pos: 0 };
            let expe = Expr::Add(
                Box::new(Mul::Primary(Box::new(Primary::Num(1)))),
                Box::new(Expr::Sub(
                    Box::new(Mul::Mul(
                        Box::new(Primary::Num(2)),
                        Box::new(Mul::Primary(Box::new(Primary::Num(3)))),
                    )),
                    Box::new(Expr::Mul(Box::new(Mul::Div(
                        Box::new(Primary::Num(4)),
                        Box::new(Mul::Primary(Box::new(Primary::Num(5)))),
                    )))),
                )),
            );
            let calc = parser.parse_expr();
            assert_eq!(expe, calc);
        }
    }
}

mod coder {
    use super::parser::*;
    // 512 is magic number
    const INIT_SP: u64 = 512;

    use crate::yas::{Imm, Register, Statement};

    pub fn code(expr: &Expr) -> Vec<Statement> {
        let mut stmts = Vec::new();
        stmts.push(Statement::Irmovq(Imm::Integer(INIT_SP), Register::RSP));
        stmts.append(&mut code_expr(expr));
        stmts
    }

    fn code_expr(expr: &Expr) -> Vec<Statement> {
        match expr {
            Expr::Mul(mul) => code_mul(mul),
            Expr::Add(mul, expr) => {
                let mut stmts = code_mul(mul);
                stmts.append(&mut code_expr(expr));
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Popq(Register::RAX));
                stmts.push(Statement::Addq(Register::RBX, Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
            Expr::Sub(mul, expr) => {
                let mut stmts = code_mul(mul);
                stmts.append(&mut code_expr(expr));
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Popq(Register::RAX));
                stmts.push(Statement::Subq(Register::RBX, Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
        }
    }

    fn code_mul(mul: &Mul) -> Vec<Statement> {
        match mul {
            Mul::Primary(primary) => code_primary(primary),
            Mul::Mul(primary, mul) => {
                let mut stmts = code_primary(primary);
                stmts.append(&mut code_mul(mul));
                // FIXME
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Popq(Register::RAX));
                stmts.push(Statement::Subq(Register::RBX, Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
            Mul::Div(primary, mul) => {
                let mut stmts = code_primary(primary);
                stmts.append(&mut code_mul(mul));
                // FIXME
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Popq(Register::RAX));
                stmts.push(Statement::Subq(Register::RBX, Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
        }
    }

    fn code_primary(primary: &Primary) -> Vec<Statement> {
        match primary {
            Primary::Num(n) => vec![
                Statement::Irmovq(Imm::Integer(*n), Register::RBX),
                Statement::Pushq(Register::RBX),
            ],
            Primary::Expr(expr) => code_expr(expr),
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_code() {
            let expr = Expr::Add(
                Box::new(Mul::Primary(Box::new(Primary::Num(1)))),
                Box::new(Expr::Mul(Box::new(Mul::Primary(Box::new(Primary::Num(2)))))),
            );
            let expe = vec![
                Statement::Irmovq(Imm::Integer(INIT_SP), Register::RSP),
                Statement::Irmovq(Imm::Integer(1), Register::RBX),
                Statement::Pushq(Register::RBX),
                Statement::Irmovq(Imm::Integer(2), Register::RBX),
                Statement::Pushq(Register::RBX),
                Statement::Popq(Register::RBX),
                Statement::Popq(Register::RAX),
                Statement::Addq(Register::RBX, Register::RAX),
                Statement::Pushq(Register::RAX),
            ];
            let calc = code(&expr);
            assert_eq!(expe, calc);
        }
    }
}

pub fn compile(src: &str) -> Vec<Statement> {
    let tokens = tokenizer::tokenize(src);
    let mut parser = parser::Parser::new(tokens);
    let expr = parser.parse();
    coder::code(&expr)
}


