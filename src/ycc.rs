/*  simple c compiler for Y86-64 */
use crate::yas::Statement;

pub mod tokenizer {
    #[derive(Debug, PartialEq)]
    pub enum Token {
        Op(char),
        Op2([char; 2]),
        Num(u64),
        Id(String),
    }

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
                    }
                }
                '+' | '-' | '*' | '/' | '(' | ')' | '<' | '>' => {
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

            let input = "13 + 2==3";
            let expe = vec![
                Token::Num(13), 
                Token::Op('+'), 
                Token::Num(2),
                Token::Op2(['=', '=']),
                Token::Num(3)
                ];
            let calc = tokenize(input);
            assert_eq!(expe, calc);
        }
    }    
}

pub mod parser {
    use super::tokenizer::Token;
    // BNF
    // expr    = mul ("+" mul | "-" mul)*
    // mul     = unary ("*" unary | "/" unary)*
    // unary   =  primary | "+" primary | "-" primary
    // primary = num | "(" expr ")"

    // context free grammer:
    // expr ::= mul | mul "+" expr | mul "-" expr

    #[derive(Debug, PartialEq)]
    pub enum Expr {
        Rel(Box<Rel>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Rel {
        Add(Box<Add>),
        Eq(Box<Rel>, Box<Add>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Add {
        Mul(Box<Mul>),
        Add(Box<Add>, Box<Mul>),
        Sub(Box<Add>, Box<Mul>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Mul {
        Unary(Box<Unary>),
        Mul(Box<Mul>, Box<Unary>),
        Div(Box<Mul>, Box<Unary>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Unary {
        Primary(Box<Primary>),
        Pos(Box<Primary>),
        Neg(Box<Primary>),
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
            let rel = self.parse_rel();
            Expr::Rel(Box::new(rel))
        }

        fn parse_rel(&mut self) -> Rel {
            let mut left = Rel::Add(Box::new(self.parse_add()));
            while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                match token {
                    Token::Op2(['=', '=']) => {
                        self.pos += 1;
                        let right = self.parse_add();
                        left = Rel::Eq(Box::new(left), Box::new(right));
                    },
                    _ => break,
                }
            }
            left
        }

        fn parse_add(&mut self) -> Add {
            let mut left = Add::Mul(Box::new(self.parse_mul()));
            while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                match token {
                    Token::Op('+') => {
                        self.pos += 1;
                        let right = self.parse_mul();
                        left = Add::Add(Box::new(left), Box::new(right));
                    },
                    Token::Op('-') => {
                        self.pos += 1;
                        let right = self.parse_mul();
                        left = Add::Sub(Box::new(left), Box::new(right));
                    },
                    _ => break,
                }
            }
            left
        }

        fn parse_mul(&mut self) -> Mul {
            let mut left = Mul::Unary(Box::new(self.parse_unary()));
            while let Some(token) = self.tokens.get(self.pos) {
                match token {
                    Token::Op('*') => {
                        self.pos += 1;
                        left = Mul::Mul(Box::new(left), Box::new(self.parse_unary()))
                    }
                    Token::Op('/') => {
                        self.pos += 1;
                        left = Mul::Div(Box::new(left), Box::new(self.parse_unary()))
                    }
                    _ => break,
                }
            }
            left
        }

        fn parse_unary(&mut self) -> Unary {
            match self.tokens.get(self.pos) {
                Some(Token::Op('+')) => {
                    self.pos += 1;
                    Unary::Pos(Box::new(self.parse_primary()))
                }
                Some(Token::Op('-')) => {
                    self.pos += 1;
                    Unary::Neg(Box::new(self.parse_primary()))
                }
                _ => Unary::Primary(Box::new(self.parse_primary())),
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

        fn num(n: u64) -> Box<Primary> {
            Box::new(Primary::Num(n))
        }
        
        fn unary1(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Primary(primary))
        }

        fn pos(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Pos(primary))
        }

        fn neg(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Neg(primary))
        }
        
        fn mul1(unary: Box<Unary>) -> Box<Mul> {
            Box::new(Mul::Unary(unary))
        }
        
        fn mul(left: Box<Mul>, right: Box<Unary>) -> Box<Mul> {
            Box::new(Mul::Mul(left, right))
        }
        
        fn div(left: Box<Mul>, right: Box<Unary>) -> Box<Mul> {
            Box::new(Mul::Div(left, right))
        }
        
        fn expr1(rel: Box<Rel>) -> Box<Expr> {
            Box::new(Expr::Rel(rel))
        }

        fn add(left: Box<Add>, right: Box<Mul>) -> Box<Add> {
            Box::new(Add::Add(left, right))
        }
        
        fn sub(left: Box<Add>, right: Box<Mul>) -> Box<Add> {
            Box::new(Add::Sub(left, right))
        }

        fn add1(add: Box<Mul>) -> Box<Add> {
            Box::new(Add::Mul(add))
        }

        fn rel1(add: Box<Add>) -> Box<Rel> {
            Box::new(Rel::Add(add))
        }

        // fn eq(left: Box<Rel>, right: Box<Add>) -> Box<Rel> {
        //     Box::new(Rel::Eq(left, right))
        // }

        #[test]
        fn test_expr() {
            let tokens = vec![
                Token::Op('+'),
                Token::Num(1),

                Token::Op('+'),

                Token::Num(2),
                Token::Op('*'),
                Token::Op('-'),
                Token::Num(3),

                Token::Op('-'),

                Token::Num(4),
                Token::Op('/'),
                Token::Num(5),
            ];
            let mut parser = Parser { tokens, pos: 0 };
            let m_1 = mul1(pos(num(1)));
            let m_2_3 = mul(
                mul1(unary1(num(2))),
                neg(num(3)));
            let m_4_5 = div(
                mul1(unary1(num(4))), 
            unary1(num(5)));
            let expe = expr1(rel1(sub(
                add(add1(m_1), m_2_3),
                    m_4_5)));
            let calc = parser.parse_expr();
            assert_eq!(expe, calc.into());
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
            Expr::Rel(rel) => code_rel(rel),
        }
    }

    fn code_rel(rel: &Rel) -> Vec<Statement> {
        match rel {
            Rel::Add(add) => code_add(add),
            Rel::Eq(rel, add) => {
                // FIXME
                let mut stmts = code_rel(rel);
                stmts.append(&mut code_add(add));
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Popq(Register::RAX));
                // stmts.push(Statement::Cmpq(Register::RBX, Register::RAX));
                stmts.push(Statement::Irmovq(Imm::Integer(0), Register::RAX));
                stmts.push(Statement::Irmovq(Imm::Integer(1), Register::RBX));
                // stmts.push(Statement::Cmovneq(Register::RBX, Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
        }
    }

    fn code_add(add: &Add) -> Vec<Statement> {
        match add {
            Add::Mul(mul) => code_mul(mul),
            Add::Add(add, mul,) => {
                let mut stmts = code_add(add);
                stmts.append(&mut code_mul(mul));
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Popq(Register::RAX));
                stmts.push(Statement::Addq(Register::RBX, Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
            Add::Sub(add, mul) => {
                let mut stmts = code_add(add);
                stmts.append(&mut code_mul(mul));
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
            Mul::Unary(unary) => code_unary(unary),
            Mul::Mul(mul, unary) => {
                let mut stmts = code_mul(mul);
                stmts.append(&mut code_unary(unary));
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Popq(Register::RAX));
                stmts.push(Statement::Mulq(Register::RBX, Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
            Mul::Div(mul, unary) => {
                let mut stmts = code_mul(mul);
                stmts.append(&mut code_unary(unary));
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Popq(Register::RAX));
                stmts.push(Statement::Divq(Register::RBX, Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
        }
    }

    fn code_unary(unary: &Unary) -> Vec<Statement> {
        match unary {
            Unary::Primary(primary) => code_primary(primary),
            Unary::Pos(primary) => code_primary(primary),
            Unary::Neg(primary) => {
                // transform -x to 0 - x
                let mut stmts = Vec::new();
                stmts.push(Statement::Irmovq(Imm::Integer(0), Register::RAX));
                stmts.push(Statement::Pushq(Register::RAX));
                stmts.append(&mut code_primary(primary));
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
            let expr = Expr::Rel(Box::new(Rel::Add(Box::new(Add::Add(
                Box::new(Add::Mul(Box::new(Mul::Unary(Box::new(Unary::Primary(Box::new(Primary::Num(1)))))))),
                Box::new(Mul::Unary(Box::new(Unary::Primary(Box::new(Primary::Num(2)))))))))));
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
    // println!("{:?}", expr);
    coder::code(&expr)
}


