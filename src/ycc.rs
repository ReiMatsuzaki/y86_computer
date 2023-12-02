/*  simple c compiler for Y86-64 */
use crate::yas::Statement;

pub mod tokenizer {
    #[derive(Debug, PartialEq)]
    pub enum Token {
        Op(char),
        Op2([char; 2]),
        Num(u64),
        Id(String),
        Return,
        While,
        If,
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
                    } else {
                        tokens.push(Token::Op('='));
                    }
                }
                '+' | '-' | '*' | '/' | '(' | ')' | '<' | '>' | ';' | '{' | '}' => {
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
            let input = "while (a < 10) { if(a ==2) {a = a + 1; return b; } }";
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
                Token::Id(String::from("a")),
                Token::Op('='),
                Token::Id(String::from("a")),
                Token::Op('+'),
                Token::Num(1),
                Token::Op(';'),
                Token::Return,
                Token::Id(String::from("b")),
                Token::Op(';'),
                Token::Op('}'),
                Token::Op('}'),
            ];
            let calc = tokenize(input);
            assert_eq!(expe, calc);
        }
    }
}

pub mod simpl {
    // program = stmt*
    // stmt    = expr ";" |
    //           "return" expr ";" |
    //           "while" "(" expr ")" stmt |
    //           "if" "(" expr ")" stmt "else" stmt
    // expr    = assign
    // assign  = rel ("=" assign)?
    // rel     = add ("==" add)*
    // add     = mul ("+" mul | "-" mul)*
    // mul     = unary ("*" unary | "/" unary)*
    // unary   = ("-")? primary
    // primary = num | ident | "(" expr ")"
    use super::tokenizer::Token;
    use crate::yas::{Imm, ModDest, Register, Statement, Dest};

    const INIT_SP: u64 = 512; // initial stack pointer
    const NUM_LVAR: u64 = 3; // number of local variable

    #[derive(Debug, PartialEq)]
    pub struct Prog {
        stmts: Vec<Box<Node>>,
    }

    impl Prog {
        pub fn display(&self) {
            println!("Prog:");
            for a in &self.stmts {
                println!("{:?}", a);
            }
        }
    }

    #[derive(Debug, PartialEq)]
    enum Node {
        BinaryOp(BinaryOp, Box<Node>, Box<Node>),
        UnaryOp(UnaryOp, Box<Node>),
        Num(u64),
        Variable(String, u64),
    }

    #[derive(Debug, PartialEq)]
    enum BinaryOp {
        Add,
        Sub,
        Mul,
        Div,
        Assign,
        Eq,
        If,
        While,
    }

    #[derive(Debug, PartialEq)]
    enum UnaryOp {
        Neg,
        Ret,
    }

    pub struct Parser {
        pub(crate) tokens: Vec<Token>,
        pos: usize,
    }

    impl Parser {
        pub fn new(tokens: Vec<Token>) -> Self {
            Parser { tokens, pos: 0 }
        }

        pub fn parse(&mut self) -> Prog {
            self.parse_prog()
        }

        fn expect(&mut self, token: &Token) {
            if self.tokens[self.pos] == *token {
                self.pos += 1;
            } else {
                panic!("expected {:?}, but found {:?}. pos={2}", token, self.tokens[self.pos], self.pos);
            }
        }

        fn parse_prog(&mut self) -> Prog {
            let mut stmts = vec![self.parse_stmt()];
            while self.pos < self.tokens.len() {
                stmts.push(self.parse_stmt());
            }
            Prog { stmts }
        }

        fn parse_stmt(&mut self) -> Box<Node> {
            match self.tokens.get(self.pos) {
                Some(Token::Return) => {
                    self.pos += 1;
                    let expr = self.parse_expr();
                    self.expect(&Token::Op(';'));
                    Box::new(Node::UnaryOp(UnaryOp::Ret, expr))
                },
                Some(Token::While) => {
                    self.pos += 1;
                    self.expect(&Token::Op('('));
                    let expr = self.parse_expr();
                    self.expect(&Token::Op(')'));
                    let stmt = self.parse_stmt();
                    Box::new(Node::BinaryOp(BinaryOp::While, expr, stmt))
                },
                Some(Token::If) => {
                    self.pos += 1;
                    self.expect(&Token::Op('('));
                    let expr = self.parse_expr();
                    self.expect(&Token::Op(')'));
                    let stmt = self.parse_stmt();
                    Box::new(Node::BinaryOp(BinaryOp::If, expr, stmt))                    
                },
                _ => {
                    let expr = self.parse_expr();
                    self.expect(&Token::Op(';'));
                    expr
                }
            }
        }

        fn parse_expr(&mut self) -> Box<Node> {
            self.parse_assign()
        }

        fn parse_assign(&mut self) -> Box<Node> {
            let left = self.parse_rel(); // FIXME
            if let Some(&Token::Op('=')) = self.tokens.get(self.pos) {
                self.pos += 1;
                let right = self.parse_rel();
                Box::new(Node::BinaryOp(BinaryOp::Assign, left, right))
            } else {
                left
            }
        }

        fn parse_rel(&mut self) -> Box<Node> {
            let mut left = self.parse_add();
            while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                match token {
                    Token::Op2(['=', '=']) => {
                        self.pos += 1;
                        let right = self.parse_add();
                        left = Box::new(Node::BinaryOp(BinaryOp::Eq, left, right));
                    }
                    _ => break,
                }
            }
            left
        }

        fn parse_add(&mut self) -> Box<Node> {
            let mut left = self.parse_mul();
            while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                match token {
                    Token::Op('+') => {
                        self.pos += 1;
                        let right = self.parse_mul();
                        left = Box::new(Node::BinaryOp(BinaryOp::Add, left, right));
                    }
                    Token::Op('-') => {
                        self.pos += 1;
                        let right = self.parse_mul();
                        left = Box::new(Node::BinaryOp(BinaryOp::Sub, left, right));
                    }
                    _ => break,
                }
            }
            left
        }

        fn parse_mul(&mut self) -> Box<Node> {
            let mut left = self.parse_unary();
            while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                match token {
                    Token::Op('*') => {
                        self.pos += 1;
                        let right = self.parse_unary();
                        left = Box::new(Node::BinaryOp(BinaryOp::Mul, left, right));
                    }
                    Token::Op('/') => {
                        self.pos += 1;
                        let right = self.parse_unary();
                        left = Box::new(Node::BinaryOp(BinaryOp::Div, left, right));
                    }
                    _ => break,
                }
            }
            left
        }

        fn parse_unary(&mut self) -> Box<Node> {
            match self.tokens.get(self.pos) {
                Some(Token::Op('-')) => {
                    self.pos += 1;
                    let primary = self.parse_primary();
                    Box::new(Node::UnaryOp(UnaryOp::Neg, primary))
                }
                _ => self.parse_primary(),
            }
        }

        fn parse_primary(&mut self) -> Box<Node> {
            match self.tokens.get(self.pos) {
                Some(Token::Num(n)) => {
                    self.pos += 1;
                    Box::new(Node::Num(*n))
                }
                Some(Token::Id(id)) => {
                    self.pos += 1;
                    let name = String::from(id);
                    let offset = 8 * (id.as_bytes()[0] - ('a' as u8) + 1) as u64;
                    Box::new(Node::Variable(name, offset))
                }
                Some(Token::Op('(')) => {
                    self.pos += 1;
                    let expr = self.parse_expr();
                    match self.tokens.get(self.pos) {
                        Some(Token::Op(')')) => {
                            self.pos += 1;
                            expr
                        }
                        _ => panic!("expected token ')'. but found {:?}", self.tokens[self.pos]),
                    }
                }
                _ => panic!("unexpected token in primary: {:?}", self.tokens[self.pos]),
            }
        }
    }

    pub struct Coder {}

    impl Coder {
        pub fn code(&self, prog: &Prog) -> Vec<Statement> {
            let mut stmts = self.code_prologue();
            for node in &prog.stmts {
                stmts.append(&mut self.code_node(node));
                stmts.push(Statement::Popq(Register::RCX));
            }
            stmts
        }

        fn code_prologue(&self) -> Vec<Statement> {
            vec![
                // initialize stack pointer and meaning less base pointer
                Statement::Irmovq(Imm::Integer(INIT_SP), Register::RSP),
                Statement::Irmovq(Imm::Integer(INIT_SP + 10), Register::RBP),

                // push artificial return address (call)
                Statement::Irmovq(Imm::Integer(1000), Register::RBX),
                Statement::Pushq(Register::RBX),

                // ==== enter function heare ====

                // save previous registers
                Statement::Pushq(Register::RBP),
                // set new base pointer
                Statement::Rrmovq(Register::RSP, Register::RBP),

                // allocate local variables
                Statement::Irmovq(Imm::Integer(NUM_LVAR * 8), Register::RAX),
                Statement::Subq(Register::RAX, Register::RSP),
            ]
        }

        fn code_node(&self, node: &Node) -> Vec<Statement> {
            match node {
                Node::BinaryOp(BinaryOp::Assign, left, right) => {
                    let mut codes = self.code_lvar(left);
                    codes.append(&mut self.code_node(right));
                    codes.push(Statement::Popq(Register::RBX));
                    codes.push(Statement::Popq(Register::RAX)); // (%rax) = %rbx
                    codes.push(Statement::Rmmovq(
                        Register::RBX,
                        ModDest {
                            dest: crate::yas::Dest::Integer(0),
                            register: Register::RAX,
                        },
                    ));
                    codes.push(Statement::Pushq(Register::RBX));
                    codes
                }
                Node::BinaryOp(BinaryOp::If, cond, then) => {
                    let false_label = "iffalse";
                    let mut codes = self.code_node(cond);
                    let mut codes_cond = vec![
                        Statement::Popq(Register::RAX),
                        Statement::Irmovq(Imm::Integer(0), Register::RBX),
                        Statement::Subq(Register::RBX, Register::RAX),
                        Statement::Pushq(Register::RAX),
                        Statement::Je(Dest::Label(false_label.to_string())),
                        Statement::Popq(Register::RAX),
                    ];
                    codes.append(&mut codes_cond);
                    codes.append(&mut self.code_node(then));
                    codes.push(Statement::Label(false_label.to_string()));
                    codes
                },
                Node::BinaryOp(BinaryOp::While, cond, then) => {
                    // FIXME: change label name to avoid conflict
                    let begin_label = "whilebegin";
                    let end_label = "whileend";
                    let mut codes = self.code_node(cond);
                    let mut codes_cond = vec![
                        Statement::Label(begin_label.to_string()),
                        Statement::Popq(Register::RAX),
                        Statement::Irmovq(Imm::Integer(0), Register::RBX),
                        Statement::Addq(Register::RBX, Register::RAX),
                        Statement::Je(Dest::Label(end_label.to_string())),
                    ];
                    codes.append(&mut codes_cond);
                    codes.append(&mut self.code_node(then));
                    // FIXME: change je -> jmp
                    codes.push(Statement::Je(Dest::Label(begin_label.to_string())));
                    codes.push(Statement::Label(end_label.to_string()));
                    codes
                },
                Node::BinaryOp(op, left, right) => {
                    let mut codes = self.code_node(left);
                    codes.append(&mut self.code_node(right));
                    codes.push(Statement::Popq(Register::RBX));
                    codes.push(Statement::Popq(Register::RAX)); // %rax OP %rbx
                    let mut ss = match op {
                        BinaryOp::Add => vec![Statement::Addq(Register::RBX, Register::RAX)],
                        BinaryOp::Sub => vec![Statement::Subq(Register::RBX, Register::RAX)],
                        BinaryOp::Mul => vec![Statement::Mulq(Register::RBX, Register::RAX)],
                        BinaryOp::Div => vec![Statement::Divq(Register::RBX, Register::RAX)],
                        BinaryOp::Eq => vec![
                            Statement::Irmovq(Imm::Integer(0), Register::RSI),
                            Statement::Irmovq(Imm::Integer(1), Register::RDI),
                            Statement::Subq(Register::RBX, Register::RAX),
                            Statement::Cmove(Register::RDI, Register::RSI),
                            Statement::Rrmovq(Register::RSI, Register::RAX),
                        ],
                        _ => panic!("unexpected binary op"),
                    };
                    codes.append(&mut ss);
                    codes.push(Statement::Pushq(Register::RAX));
                    codes
                }
                Node::UnaryOp(UnaryOp::Ret, node) => {
                    let mut codes = self.code_node(node);
                    let mut ss = vec![
                        // remove current result
                        Statement::Popq(Register::RAX),

                        // deallocate local variables
                        Statement::Irmovq(Imm::Integer(NUM_LVAR * 8), Register::RBX),
                        Statement::Addq(Register::RBX, Register::RSP),

                        // restore old register
                        Statement::Popq(Register::RBP),

                        // return
                        Statement::Ret
                    ];
                    codes.append(&mut ss);
                    codes
                }
                Node::UnaryOp(UnaryOp::Neg, unode) => {
                    // replace -x => 0 - x
                    let mut codes = self.code_node(unode);
                    let mut ss = vec![
                        Statement::Popq(Register::RBX),
                        Statement::Irmovq(Imm::Integer(0), Register::RAX),
                        Statement::Subq(Register::RBX, Register::RAX), // 0 sub %rbx
                        Statement::Pushq(Register::RAX),
                    ];
                    codes.append(&mut ss);
                    codes
                }
                Node::Num(n) => {
                    vec![
                        Statement::Irmovq(Imm::Integer(*n), Register::RBX),
                        Statement::Pushq(Register::RBX),
                    ]
                }
                Node::Variable(_, _) => {
                    let mut codes = self.code_lvar(node);
                    codes.push(Statement::Popq(Register::RAX));
                    codes.push(Statement::Mrmovq(
                        ModDest {
                            dest: crate::yas::Dest::Integer(0),
                            register: Register::RAX,
                        },
                        Register::RAX,
                    ));
                    codes.push(Statement::Pushq(Register::RAX));
                    codes
                }
            }
        }

        fn code_lvar(&self, node: &Node) -> Vec<Statement> {
            match node {
                Node::Variable(_, offset) => {
                    vec![
                        Statement::Irmovq(Imm::Integer(*offset), Register::RAX),
                        Statement::Rrmovq(Register::RBP, Register::RBX),
                        Statement::Subq(Register::RAX, Register::RBX),
                        Statement::Pushq(Register::RBX),
                    ]
                }
                _ => panic!("unexpected node in code_lvar"),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_parser() {
            let tokens = vec![
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
                Token::Op(';'),
            ];
            let mut parser = Parser { tokens, pos: 0 };
            let m_1 = num(1);
            let m_2_3 = mul(num(2), neg(num(3)));
            let m_4_5 = div(num(4), num(5));
            let expe = sub(add(m_1, m_2_3), m_4_5);
            let expe = Prog { stmts: vec![expe] };
            let calc = parser.parse_prog();
            assert_eq!(expe, calc);
        }

        #[test]
        fn test_parse_statement() {
            let tokens = vec![
                Token::If,
                Token::Op('('),
                Token::Id(String::from("a")),
                Token::Op2(['=', '=']),
                Token::Num(2),
                Token::Op(')'),
                Token::Id(String::from("b")),
                Token::Op('='),
                Token::Num(1),
                Token::Op(';'),
            ];
            let mut parser = Parser { tokens, pos: 0 };
            let expe = Prog {
                stmts: vec![Box::new(Node::BinaryOp(
                    BinaryOp::If,
                    Box::new(Node::BinaryOp(
                        BinaryOp::Eq,
                        Box::new(Node::Variable(String::from("a"), 8)),
                        Box::new(Node::Num(2)),
                    )),
                    Box::new(Node::BinaryOp(
                        BinaryOp::Assign,
                        Box::new(Node::Variable(String::from("b"), 16)),
                        Box::new(Node::Num(1)),
                    )),
                ))],
            };
            let calc = parser.parse_prog();
            assert_eq!(expe, calc);
        }


        #[test]
        fn test_code() {
            let coder = Coder {};
            // a = 1 + 2
            let a = add(num(23), num(34));
            let p = Prog { stmts: vec![a]};
            let mut expe = coder.code_prologue();
            expe.append(&mut vec![
                Statement::Irmovq(Imm::Integer(23), Register::RBX),
                Statement::Pushq(Register::RBX),
                Statement::Irmovq(Imm::Integer(34), Register::RBX),
                Statement::Pushq(Register::RBX),
                Statement::Popq(Register::RBX),
                Statement::Popq(Register::RAX),
                Statement::Addq(Register::RBX, Register::RAX),
                Statement::Pushq(Register::RAX),

                Statement::Popq(Register::RCX),
            ]);
            let calc = coder.code(&p);
            assert_eq!(expe, calc);
        }

        fn num(n: u64) -> Box<Node> {
            Box::new(Node::Num(n))
        }

        fn neg(node: Box<Node>) -> Box<Node> {
            Box::new(Node::UnaryOp(UnaryOp::Neg, node))
        }

        fn mul(left: Box<Node>, right: Box<Node>) -> Box<Node> {
            Box::new(Node::BinaryOp(BinaryOp::Mul, left, right))
        }

        fn div(left: Box<Node>, right: Box<Node>) -> Box<Node> {
            Box::new(Node::BinaryOp(BinaryOp::Div, left, right))
        }

        fn add(left: Box<Node>, right: Box<Node>) -> Box<Node> {
            Box::new(Node::BinaryOp(BinaryOp::Add, left, right))
        }

        fn sub(left: Box<Node>, right: Box<Node>) -> Box<Node> {
            Box::new(Node::BinaryOp(BinaryOp::Sub, left, right))
        }

        // fn eq(left: Box<Node>, right: Box<Node>) -> Box<Node> {
        //     Box::new(Node::BinaryOp(BinaryOp::Eq, left, right))
        // }

        // fn assign(left: Box<Node>, right: Box<Node>) -> Box<Node> {
        //     Box::new(Node::BinaryOp(BinaryOp::Assign, left, right))
        // }
    }
}

pub fn scompile(src: &str, verbose: i64) -> Vec<Statement> {
    let tokens = tokenizer::tokenize(src);
    if verbose >= 1 {
        println!("tokens: {:?}", tokens)
    }
    let mut parser = simpl::Parser::new(tokens);
    let prog = parser.parse();
    if verbose >= 1 {
        prog.display();
    }
    let coder = simpl::Coder {};
    let codes = coder.code(&prog);
    codes
}
