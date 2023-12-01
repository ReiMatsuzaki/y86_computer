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
                '+' | '-' | '*' | '/' | '(' | ')' | '<' | '>' | ';' => {
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
                Token::Num(3),
            ];
            let calc = tokenize(input);
            assert_eq!(expe, calc);
        }
    }
}

pub mod simpl {
    // program = stmt*
    // stmt    = expr ";"
    // expr    = assign
    // assign  = rel ("=" assign)?
    // rel     = add ("==" add)*
    // add     = mul ("+" mul | "-" mul)*
    // mul     = unary ("*" unary | "/" unary)*
    // unary   = ("-")? primary
    // primary = num | ident | "(" expr ")"
    use super::{tokenizer::Token, parser::Stmt};
    use crate::yas::{Imm, ModDest, Register, Statement};

    const INIT_SP: u64 = 512; // initial stack pointer
    const NUM_LVAR: u64 = 10; // number of local variable

    #[derive(Debug, PartialEq)]
    pub struct Prog {
        stmts: Vec<Box<Node>>,
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
    }

    #[derive(Debug, PartialEq)]
    enum UnaryOp {
        Neg,
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

        fn parse_prog(&mut self) -> Prog {
            let mut stmts = vec![self.parse_stmt()];
            while let Some(&Token::Op(';')) = self.tokens.get(self.pos) {
                self.pos += 1;
                stmts.push(self.parse_stmt());
            }
            Prog { stmts }
        }

        fn parse_stmt(&mut self) -> Box<Node> {
            let expr = self.parse_expr();
            if let Some(&Token::Op(';')) = self.tokens.get(self.pos) {
                self.pos += 1;
            } else {
                panic!("expected ';', but found {:?}", self.tokens[self.pos]);
            }
            expr
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
                    let offset = (id.as_bytes()[0] - ('a' as u8)) as u64;
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
            }
            stmts.append(&mut self.code_epilogue());

            stmts
        }

        fn code_prologue(&self) -> Vec<Statement> {
            vec![
                // initialize stack pointer and meaning less base pointer
                Statement::Irmovq(Imm::Integer(INIT_SP), Register::RSP),
                Statement::Irmovq(Imm::Integer(INIT_SP + 10), Register::RBP),
                // Prologue
                // save base pointer and set current stack pointer to base pointer
                Statement::Pushq(Register::RBP),
                Statement::Rrmovq(Register::RSP, Register::RBP),
                // allocate local variables
                Statement::Irmovq(Imm::Integer(NUM_LVAR * 8), Register::RAX),
                Statement::Subq(Register::RAX, Register::RSP),
            ]
        }

        fn code_epilogue(&self) -> Vec<Statement> {
            vec![
                // deallocate local variables
                Statement::Rrmovq(Register::RBP, Register::RSP),
                // restore base pointer
                Statement::Popq(Register::RBP),
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
                            Statement::Cmove(Register::RSI, Register::RAX),
                        ],
                        _ => panic!("unexpected binary op"),
                    };
                    codes.append(&mut ss);
                    codes.push(Statement::Pushq(Register::RAX));
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
                _ => panic!("unexpected node in assign"),
            }
        }

        fn code_lvar(&self, node: &Node) -> Vec<Statement> {
            match node {
                Node::Variable(s, offset) => {
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
            ]);
            expe.append(&mut coder.code_epilogue());
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

        fn eq(left: Box<Node>, right: Box<Node>) -> Box<Node> {
            Box::new(Node::BinaryOp(BinaryOp::Eq, left, right))
        }

        fn assign(left: Box<Node>, right: Box<Node>) -> Box<Node> {
            Box::new(Node::BinaryOp(BinaryOp::Assign, left, right))
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

    #[derive(Debug, PartialEq)]
    pub enum Prog {
        Stmt(Box<Stmt>),
        Mult(Box<Stmt>, Box<Prog>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Stmt {
        Expr(Box<Expr>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Expr {
        Assign(Box<Assign>),
    }

    #[derive(Debug, PartialEq)]
    pub enum Assign {
        Rel(Box<Rel>),
        Assign(Box<Rel>, Box<Rel>),
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
        Var(Var),
        Expr(Box<Expr>),
    }

    #[derive(Debug, PartialEq)]
    pub struct Var {
        pub name: String,
        pub offset: u64,
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

        fn parse_prog(&mut self) -> Prog {
            let stmt = self.parse_stmt();
            if let Some(&Token::Op(';')) = self.tokens.get(self.pos) {
                self.pos += 1;
                let prog = self.parse_prog();
                Prog::Mult(Box::new(stmt), Box::new(prog))
            } else {
                Prog::Stmt(Box::new(stmt))
            }
        }

        fn parse_stmt(&mut self) -> Stmt {
            let expr = self.parse_expr();
            Stmt::Expr(Box::new(expr))
        }

        fn parse_expr(&mut self) -> Expr {
            let assign = self.parse_assign();
            Expr::Assign(Box::new(assign))
        }

        fn parse_assign(&mut self) -> Assign {
            let left = self.parse_rel();
            if let Some(&Token::Op('=')) = self.tokens.get(self.pos) {
                self.pos += 1;
                let right = self.parse_rel();
                Assign::Assign(Box::new(left), Box::new(right))
            } else {
                Assign::Rel(Box::new(left))
            }
        }

        fn parse_rel(&mut self) -> Rel {
            let mut left = Rel::Add(Box::new(self.parse_add()));
            while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                match token {
                    Token::Op2(['=', '=']) => {
                        self.pos += 1;
                        let right = self.parse_add();
                        left = Rel::Eq(Box::new(left), Box::new(right));
                    }
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
                    }
                    Token::Op('-') => {
                        self.pos += 1;
                        let right = self.parse_mul();
                        left = Add::Sub(Box::new(left), Box::new(right));
                    }
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
                Some(Token::Id(id)) => {
                    self.pos += 1;
                    // FIXME
                    let offset = (id.as_bytes()[0] - ('a' as u8)) as u64;
                    Primary::Var(Var {
                        name: id.to_string(),
                        offset,
                    })
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
        pub fn num(n: u64) -> Box<Primary> {
            Box::new(Primary::Num(n))
        }

        pub fn unary1(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Primary(primary))
        }

        pub fn pos(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Pos(primary))
        }

        pub fn neg(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Neg(primary))
        }

        pub fn mul1(unary: Box<Unary>) -> Box<Mul> {
            Box::new(Mul::Unary(unary))
        }

        pub fn mul(left: Box<Unary>, right: Box<Unary>) -> Box<Mul> {
            let left = Box::new(Mul::Unary(left));
            Box::new(Mul::Mul(left, right))
        }

        pub fn div(left: Box<Unary>, right: Box<Unary>) -> Box<Mul> {
            let left = Box::new(Mul::Unary(left));
            Box::new(Mul::Div(left, right))
        }

        pub fn add(left: Box<Mul>, right: Box<Mul>) -> Box<Add> {
            let l = Box::new(Add::Mul(left));
            Box::new(Add::Add(l, right))
        }

        pub fn sub(left: Box<Mul>, right: Box<Mul>) -> Box<Add> {
            let l = Box::new(Add::Mul(left));
            Box::new(Add::Sub(l, right))
        }

        pub fn add1(add: Box<Mul>) -> Box<Add> {
            Box::new(Add::Mul(add))
        }

        pub fn rel1(add: Box<Add>) -> Box<Rel> {
            Box::new(Rel::Add(add))
        }

        pub fn eq(left: Box<Add>, right: Box<Add>) -> Box<Rel> {
            let l = Box::new(Rel::Add(left));
            Box::new(Rel::Eq(l, right))
        }

        pub fn assign1(rel: Box<Rel>) -> Box<Assign> {
            Box::new(Assign::Rel(rel))
        }

        pub fn assign(left: Box<Rel>, right: Box<Rel>) -> Box<Assign> {
            Box::new(Assign::Assign(left, right))
        }

        pub fn expr1(assign: Box<Assign>) -> Box<Expr> {
            Box::new(Expr::Assign(assign))
        }

        pub fn stmt1(expr: Box<Expr>) -> Box<Stmt> {
            Box::new(Stmt::Expr(expr))
        }

        pub fn prog1(stmt: Box<Stmt>) -> Box<Prog> {
            Box::new(Prog::Stmt(stmt))
        }

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
            let m_2_3 = mul(unary1(num(2)), neg(num(3)));
            let m_4_5 = div(unary1(num(4)), unary1(num(5)));
            let expe = Add::Sub(add(m_1, m_2_3), m_4_5);
            let expe = prog1(stmt1(expr1(assign1(rel1(Box::new(expe))))));
            let calc = parser.parse_prog();
            assert_eq!(expe, calc.into());
        }
    }
}

mod coder {
    use super::parser::*;
    const INIT_SP: u64 = 512; // initial stack pointer
    const NUM_LVAR: u64 = 10; // number of local variable

    use crate::yas::{Imm, ModDest, Register, Statement};

    pub fn code(prog: &Prog) -> Vec<Statement> {
        // Artificial initializations
        let mut stmts = vec![
            // initialize stack pointer
            Statement::Irmovq(Imm::Integer(INIT_SP), Register::RSP),
            Statement::Irmovq(Imm::Integer(INIT_SP + 10), Register::RSP),
        ];

        // Prologue
        stmts.append(&mut vec![
            // save base pointer and set current stack pointer to base pointer
            Statement::Pushq(Register::RBP),
            Statement::Rrmovq(Register::RSP, Register::RBP),
            // allocate local variables
            Statement::Irmovq(Imm::Integer(NUM_LVAR * 8), Register::RAX),
            Statement::Subq(Register::RAX, Register::RSP),
        ]);

        // main code
        stmts.append(&mut code_prog(prog));

        // Epilogue
        stmts.append(&mut vec![
            // deallocate local variables
            Statement::Rrmovq(Register::RBP, Register::RSP),
            // restore base pointer
            Statement::Popq(Register::RBP),
        ]);

        stmts
    }

    fn code_prog(prog: &Prog) -> Vec<Statement> {
        match prog {
            Prog::Stmt(stmt) => code_stmt(stmt),
            Prog::Mult(stmt, prog) => {
                let mut stmts = code_stmt(stmt);
                stmts.append(&mut code_prog(prog));
                stmts
            }
        }
    }

    fn code_stmt(stmt: &Stmt) -> Vec<Statement> {
        match stmt {
            Stmt::Expr(expr) => code_expr(expr),
        }
    }

    fn code_expr(expr: &Expr) -> Vec<Statement> {
        match expr {
            Expr::Assign(assign) => code_assign(assign),
        }
    }

    fn code_assign(assign: &Assign) -> Vec<Statement> {
        // FIXME
        match assign {
            Assign::Rel(rel) => code_rel(rel),
            Assign::Assign(left, right) => {
                // match left {
                //     Rel::Add(Box::<Add>(Add::Mul(m))) => panic!("unexpected assign left hand"),
                //     // Rel::Add(Box<Add>(Add::Mul(Box<Mul>::Unary(Box<Unary>::Primary(Box<Primary>::Var(var))))) => {
                //     //     let mut stmts = code_lvar(v);
                //     // },
                //     _ => panic!("unexpected assign left hand"),
                // };
                let mut stmts = code_rel(left);
                stmts.append(&mut code_rel(right));
                stmts.push(Statement::Popq(Register::RBX)); // right hand: value
                stmts.push(Statement::Popq(Register::RAX)); // left hand : variable address
                let d = ModDest {
                    dest: crate::yas::Dest::Integer(0),
                    register: Register::RAX,
                };
                stmts.push(Statement::Rmmovq(Register::RBX, d));
                stmts.push(Statement::Pushq(Register::RBX));
                stmts
            }
        }
    }

    fn code_rel(rel: &Rel) -> Vec<Statement> {
        match rel {
            Rel::Add(add) => code_add(add),
            Rel::Eq(rel, add) => {
                let mut stmts = code_rel(rel);
                stmts.append(&mut code_add(add));
                stmts.push(Statement::Irmovq(Imm::Integer(0), Register::RAX));
                stmts.push(Statement::Irmovq(Imm::Integer(1), Register::RDI));

                stmts.push(Statement::Popq(Register::RCX));
                stmts.push(Statement::Popq(Register::RBX));
                stmts.push(Statement::Subq(Register::RBX, Register::RCX));
                stmts.push(Statement::Cmove(Register::RDI, Register::RAX));

                stmts.push(Statement::Pushq(Register::RAX));
                stmts
            }
        }
    }

    fn code_add(add: &Add) -> Vec<Statement> {
        match add {
            Add::Mul(mul) => code_mul(mul),
            Add::Add(add, mul) => {
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
            Primary::Var(var) => {
                let mut stmts = code_lvar(var);
                let d = ModDest {
                    dest: crate::yas::Dest::Integer(0),
                    register: Register::RAX,
                };
                let mut s = vec![
                    Statement::Popq(Register::RAX), // pop variable address
                    Statement::Mrmovq(d, Register::RAX),
                    Statement::Pushq(Register::RAX), // push variable value
                ];
                stmts.append(&mut s);
                stmts
            }
            Primary::Expr(expr) => code_expr(expr),
        }
    }

    fn code_lvar(var: &Var) -> Vec<Statement> {
        vec![
            Statement::Irmovq(Imm::Integer(var.offset), Register::RAX),
            Statement::Rrmovq(Register::RBP, Register::RBX),
            Statement::Subq(Register::RAX, Register::RBX),
            Statement::Pushq(Register::RBX), //push variable address(=base_pointer - offset)
        ]
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        pub fn num(n: u64) -> Box<Primary> {
            Box::new(Primary::Num(n))
        }

        pub fn unary1(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Primary(primary))
        }

        pub fn pos(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Pos(primary))
        }

        pub fn neg(primary: Box<Primary>) -> Box<Unary> {
            Box::new(Unary::Neg(primary))
        }

        pub fn mul1(unary: Box<Unary>) -> Box<Mul> {
            Box::new(Mul::Unary(unary))
        }

        // pub fn mul(left: Box<Unary>, right: Box<Unary>) -> Box<Mul> {
        //     let left = Box::new(Mul::Unary(left));
        //     Box::new(Mul::Mul(left, right))
        // }

        // pub fn div(left: Box<Unary>, right: Box<Unary>) -> Box<Mul> {
        //     let left = Box::new(Mul::Unary(left));
        //     Box::new(Mul::Div(left, right))
        // }

        pub fn add(left: Box<Mul>, right: Box<Mul>) -> Box<Add> {
            let l = Box::new(Add::Mul(left));
            Box::new(Add::Add(l, right))
        }

        // pub fn sub(left: Box<Mul>, right: Box<Mul>) -> Box<Add> {
        //     let l = Box::new(Add::Mul(left));
        //     Box::new(Add::Sub(l, right))
        // }

        // pub fn add1(add: Box<Mul>) -> Box<Add> {
        //     Box::new(Add::Mul(add))
        // }

        pub fn rel1(add: Box<Add>) -> Box<Rel> {
            Box::new(Rel::Add(add))
        }

        // pub fn eq(left: Box<Add>, right: Box<Add>) -> Box<Rel> {
        //     let l = Box::new(Rel::Add(left));
        //     Box::new(Rel::Eq(l, right))
        // }

        pub fn assign1(rel: Box<Rel>) -> Box<Assign> {
            Box::new(Assign::Rel(rel))
        }

        // pub fn assign(left: Box<Rel>, right: Box<Rel>) -> Box<Assign> {
        //     Box::new(Assign::Assign(left, right))
        // }

        pub fn expr1(assign: Box<Assign>) -> Box<Expr> {
            Box::new(Expr::Assign(assign))
        }

        pub fn stmt1(expr: Box<Expr>) -> Box<Stmt> {
            Box::new(Stmt::Expr(expr))
        }

        pub fn prog1(stmt: Box<Stmt>) -> Box<Prog> {
            Box::new(Prog::Stmt(stmt))
        }

        #[test]
        fn test_code() {
            let a = add(mul1(pos(num(1))), mul1(pos(num(2))));
            let p = prog1(stmt1(expr1(assign1(rel1(a)))));
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
            let calc = code(&p);
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

pub fn scompile(src: &str) -> Vec<Statement> {
    let tokens = tokenizer::tokenize(src);
    let mut parser = simpl::Parser::new(tokens);
    let prog = parser.parse();
    let coder = simpl::Coder {};
    let codes = coder.code(&prog);
    codes
}
