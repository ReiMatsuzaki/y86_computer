/*  simple c compiler for Y86-64 */
use crate::yas::Statement;

pub mod tokenizer {
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Op(char),
        Op2([char; 2]),
        Num(u64),
        Id(String),
        Int,
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
                '+' | '-' | '*' | '/' | '(' | ')' | '<' | '>' | ';' | '{' | '}' | ',' => {
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
}

pub mod simpl {
    // program = def
    // def     = name "(" (ident ("," ident)*)? ")" "{" stmt* "}"
    // stmt    = expr ";" |
    //           "int" var ";" |
    //           "return" expr ";" |
    //           "while" "(" expr ")" stmt |
    //           "if" "(" expr ")" stmt "else" stmt
    //           "{" stmt* "}"
    // expr    = assign
    // assign  = rel ("=" assign)?
    // rel     = add ("==" add)*
    // add     = mul ("+" mul | "-" mul)*
    // mul     = unary ("*" unary | "/" unary)*
    // unary   = ("-")? primary
    // primary = num | ident | "(" expr ")"
    use super::tokenizer::Token;
    use crate::yas::{Dest, Imm, ModDest, Register, Statement};

    pub const INIT_SP: u64 = 2816; // initial stack pointer

    #[derive(Debug, PartialEq)]
    pub struct Prog {
        node: Box<Node>,
    }

    impl Prog {
        pub fn display(&self) {
            println!("Prog:");
            let node = &(*self.node);
            match node {
                Node::Block(stmts) => {
                    for a in stmts {
                        println!("{:?}", a);
                    }
                }
                _ => panic!("unexpected node in Prog"),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    enum Node {
        BinaryOp(BinaryOp, Box<Node>, Box<Node>),
        UnaryOp(UnaryOp, Box<Node>),
        Num(u64),
        Variable(String, i64), // name, offset; variable address is (offset + %RBP)
        DefVar(Type, String),
        Block(Vec<Box<Node>>),
        DefFun(String, Vec<String>, Box<Node>, usize), // name, args(String), block, num_lvar
        Call(String, Vec<Box<Node>>),                  // name, args(each Node is Expr)
    }

    #[derive(Debug, PartialEq)]
    enum BinaryOp {
        Add,
        Sub,
        Mul,
        Div,
        Assign,
        Eq,
        Less,
        If,
        While,
    }

    #[derive(Debug, PartialEq)]
    enum UnaryOp {
        Neg,
        Ret,
    }

    #[derive(Debug, PartialEq)]
    enum Type {
        Int,
    }

    pub struct Parser {
        pub(crate) tokens: Vec<Token>,
        pos: usize,
        args: Vec<String>,
        lvars: Vec<String>,
    }

    impl Parser {
        pub fn new(tokens: Vec<Token>) -> Self {
            Parser {
                tokens,
                pos: 0,
                args: vec![],
                lvars: vec![],
            }
        }

        pub fn parse(&mut self) -> Prog {
            self.parse_prog()
        }

        fn expect(&mut self, token: &Token) -> &mut Self {
            if self.tokens[self.pos] == *token {
                self.pos += 1;
                self
            } else {
                panic!(
                    "expected {:?}, but found {:?}. pos={2}",
                    token, self.tokens[self.pos], self.pos
                );
            }
        }

        fn parse_prog(&mut self) -> Prog {
            let mut stmts = vec![];
            while self.pos < self.tokens.len() {
                stmts.push(self.parse_deffun());
            }
            let node = Box::new(Node::Block(stmts));
            Prog { node }
        }

        fn parse_deffun(&mut self) -> Box<Node> {
            match self.tokens.get(self.pos) {
                Some(Token::Id(id)) => {
                    let name = String::from(id);
                    self.pos += 1;
                    self.expect(&Token::Op('('));
                    let mut args = vec![];
                    while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                        match token {
                            Token::Op(')') => {
                                self.pos += 1;
                                break;
                            }
                            Token::Id(s) => {
                                args.push(String::from(s));
                                self.pos += 1;
                                if let Some(&token) = self.tokens.get(self.pos).as_ref() {
                                    match token {
                                        Token::Op(')') => {
                                            self.pos += 1;
                                            break;
                                        }
                                        Token::Op(',') => {
                                            self.pos += 1;
                                        }
                                        _ => {
                                            panic!("unexpected token in function def: {:?}", token)
                                        }
                                    }
                                }
                            }
                            _ => panic!("unexpected token in function def: {:?}", token),
                        }
                    }

                    // init lvars and args. Pushed them in self.parse_stmt()
                    self.lvars = vec![];
                    self.args = args;
                    let block = self.parse_stmt();
                    let num_lvar = self.lvars.len();
                    let args = self.args.clone();
                    Box::new(Node::DefFun(name, args, block, num_lvar))
                }
                Some(_) => panic!("invalid token, pos={0}", self.pos),
                None => panic!("token not found. pos={}", self.pos),
            }
        }

        fn parse_stmt(&mut self) -> Box<Node> {
            match self.tokens.get(self.pos) {
                Some(Token::Op('{')) => {
                    self.pos += 1;
                    let mut stmts = vec![self.parse_stmt()];
                    while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                        match token {
                            Token::Op('}') => {
                                self.pos += 1;
                                break;
                            }
                            _ => stmts.push(self.parse_stmt()),
                        }
                    }
                    Box::new(Node::Block(stmts))
                }
                Some(Token::Return) => {
                    self.pos += 1;
                    let expr = self.parse_expr();
                    self.expect(&Token::Op(';'));
                    Box::new(Node::UnaryOp(UnaryOp::Ret, expr))
                }
                Some(Token::While) => {
                    self.pos += 1;
                    self.expect(&Token::Op('('));
                    let expr = self.parse_expr();
                    self.expect(&Token::Op(')'));
                    let stmt = self.parse_stmt();
                    Box::new(Node::BinaryOp(BinaryOp::While, expr, stmt))
                }
                Some(Token::If) => {
                    self.pos += 1;
                    self.expect(&Token::Op('('));
                    let expr = self.parse_expr();
                    self.expect(&Token::Op(')'));
                    let stmt = self.parse_stmt();
                    Box::new(Node::BinaryOp(BinaryOp::If, expr, stmt))
                }
                Some(Token::Int) => {
                    self.pos += 1;
                    if let Some(Token::Id(id)) = self.tokens.get(self.pos).cloned() {
                        self.pos += 1;
                        if self.lvars.contains(&id) {
                            panic!("variable already defined");
                        }
                        self.expect(&Token::Op(';'));
                        self.lvars.push(id.to_string());
                        Box::new(Node::DefVar(Type::Int, id.to_string())) // FIXME: Node::DefVar is not needed
                    } else {
                        panic!("unexpected token in defvar")
                    }
                }
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
                    Token::Op('<') => {
                        self.pos += 1;
                        let right = self.parse_add();
                        left = Box::new(Node::BinaryOp(BinaryOp::Less, left, right));
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
                    if let Some(Token::Op('(')) = self.tokens.get(self.pos) {
                        // function call
                        self.pos += 1;
                        let mut args = vec![];
                        while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                            match token {
                                Token::Op(')') => {
                                    self.pos += 1;
                                    break;
                                }
                                Token::Op(',') => {
                                    self.pos += 1;
                                }
                                _ => args.push(self.parse_expr()),
                            }
                        }
                        Box::new(Node::Call(name, args))
                    } else {
                        self.find_var(&id)
                    }
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
                _ => panic!(
                    "unexpected token in primary: {:?}, pos={1}",
                    self.tokens[self.pos], self.pos
                ),
            }
        }

        fn find_var(&self, id: &str) -> Box<Node> {
            // variable
            //                      +-(RBP)
            // stack = .. L2 L1 L0 OB RE A0 A1 A2 ..
            let name = String::from(id);
            let argi = self.args.iter().position(|x| x == id);
            let lvari = self.lvars.iter().position(|x| x == id);
            let offset = match (argi, lvari) {
                (Some(i), None) => 8 * (2 + i as i64),
                (None, Some(i)) => -8 * (1 + i as i64),
                _ => panic!("lvar not found. name={}. pos={}", id, self.pos),
            };
            Box::new(Node::Variable(name, offset))
        }
    }

    pub struct Coder {}

    impl Coder {
        pub fn code(&self, prog: &Prog) -> Vec<Statement> {
            let mut stmts = self.code_prologue();
            stmts.append(&mut self.code_stmt(&prog.node));
            stmts
        }

        fn code_prologue(&self) -> Vec<Statement> {
            let mut codes = vec![
                // initialize stack pointer and meaning less base pointer
                Statement::Irmovq(Imm::Integer(INIT_SP), Register::RSP),
                Statement::Irmovq(Imm::Integer(INIT_SP + 10), Register::RBP),
            ];
            let n = Box::new(Node::Call("main".to_string(), vec![]));
            codes.append(&mut self.code_expr(&n));
            codes.append(&mut vec![Statement::Halt]);
            codes
        }

        fn code_stmt(&self, node: &Node) -> Vec<Statement> {
            // parse node as stmt. number of stack is presevered. exception is ret.
            match node {
                // FIXME:: DefVar may be unnecessary
                Node::DefVar(_, _) => vec![],
                Node::DefFun(name, _, block, num_lvar) => {
                    let mut codes = vec![
                        // stack = .. .. .. .. RE (RE is return addressed)
                        // jump label
                        Statement::Label(name.to_string()),
                        // save previous registers
                        Statement::Pushq(Register::RBP),
                        // stack = .. .. .. OB RE (OB is old base pointer)
                        // set new base pointer
                        Statement::Rrmovq(Register::RSP, Register::RBP),
                        // allocate local variables  (deallocating is done in ret)
                        Statement::Irmovq(Imm::Integer(*num_lvar as u64 * 8), Register::RAX),
                        Statement::Subq(Register::RAX, Register::RSP),
                        // stack = .. L2 L1 OB RE (LNs are local variable)
                    ];
                    codes.append(&mut self.code_stmt(block));
                    codes
                }
                Node::Block(stmts) => {
                    let mut codes = vec![];
                    for stmt in stmts {
                        codes.append(&mut self.code_stmt(stmt));
                    }
                    codes
                }
                Node::BinaryOp(BinaryOp::If, cond, then) => {
                    // FIXME: change label name to avoid conflict
                    let false_label = "iffalse";
                    let mut codes = vec![];
                    codes.append(&mut self.code_expr(cond));
                    codes.append(&mut vec![
                        Statement::Popq(Register::RAX),
                        Statement::Irmovq(Imm::Integer(0), Register::RBX),
                        Statement::Subq(Register::RBX, Register::RAX),
                        Statement::Je(Dest::Label(false_label.to_string())),
                    ]);
                    codes.append(&mut self.code_stmt(then));
                    codes.append(&mut vec![Statement::Label(false_label.to_string())]);
                    codes
                }
                Node::BinaryOp(BinaryOp::While, cond, then) => {
                    // FIXME: change label name to avoid conflict
                    let begin_label = "whilebegin";
                    let end_label = "whileend";
                    let mut codes = vec![];
                    codes.append(&mut vec![Statement::Label(begin_label.to_string())]);
                    codes.append(&mut self.code_expr(cond));
                    codes.append(&mut vec![
                        Statement::Popq(Register::RAX),
                        Statement::Irmovq(Imm::Integer(0), Register::RBX),
                        Statement::Addq(Register::RBX, Register::RAX),
                        Statement::Je(Dest::Label(end_label.to_string())),
                    ]);
                    codes.append(&mut self.code_stmt(then));
                    codes.append(&mut vec![
                        Statement::Jmp(Dest::Label(begin_label.to_string())),
                        Statement::Label(end_label.to_string()),
                    ]);
                    codes
                }
                Node::UnaryOp(UnaryOp::Ret, node) => {
                    let mut codes = vec![];
                    codes.append(&mut self.code_expr(node));
                    codes.append(&mut vec![
                        // stack = RV L2 L1 OB RE (RV=resultant value. OB is old base pointer. RE is return address)
                        // remove current result
                        Statement::Popq(Register::RAX),
                        // stack = .. L2 L1 OB RE
                        // deallocate local variables
                        Statement::Rrmovq(Register::RBP, Register::RSP),
                        // stack = .. .. .. OB RE
                        // restore old register
                        Statement::Popq(Register::RBP),
                        // stack = .. .. .. .. RE
                        Statement::Ret,
                    ]);
                    codes
                }
                _ => {
                    let mut codes = self.code_expr(node);
                    codes.push(Statement::Popq(Register::RAX));
                    codes
                }
            }
        }

        fn code_expr(&self, node: &Node) -> Vec<Statement> {
            // parse node as expr and remain result at stack top
            match node {
                Node::BinaryOp(BinaryOp::Assign, left, right) => {
                    let mut codes = self.code_lvar(left);
                    codes.append(&mut self.code_expr(right));
                    codes.append(&mut vec![
                        Statement::Popq(Register::RBX),
                        Statement::Popq(Register::RAX), // (%rax) = %rbx
                        Statement::Rmmovq(
                            Register::RBX,
                            ModDest {
                                dest: crate::yas::Dest::Integer(0),
                                register: Register::RAX,
                            },
                        ),
                        Statement::Pushq(Register::RBX),
                    ]);
                    codes
                }
                Node::BinaryOp(op, left, right) => {
                    let mut codes = self.code_expr(left);
                    codes.append(&mut self.code_expr(right));
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
                        BinaryOp::Less => vec![
                            Statement::Irmovq(Imm::Integer(0), Register::RSI),
                            Statement::Irmovq(Imm::Integer(1), Register::RDI),
                            Statement::Subq(Register::RBX, Register::RAX),
                            Statement::Cmovl(Register::RDI, Register::RSI),
                            Statement::Rrmovq(Register::RSI, Register::RAX),
                        ],
                        _ => panic!("unexpected binary op"),
                    };
                    codes.append(&mut ss);
                    codes.push(Statement::Pushq(Register::RAX));
                    codes
                }
                Node::UnaryOp(UnaryOp::Neg, unode) => {
                    // replace -x => 0 - x
                    let mut codes = self.code_expr(unode);
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
                Node::Call(name, args) => {
                    let mut codes = vec![];
                    // stack = .. .. .. .. ..
                    // push args on stack
                    for i in 0..args.len() {
                        codes.append(&mut self.code_expr(&args[i]));
                    }
                    // stack = .. .. .. a1 a2   (a1 and a2 are argument)
                    // call
                    codes.push(Statement::Call(Dest::Label(name.to_string())));
                    // stack = .. .. .. a1 a2
                    // pop args on stack
                    for _ in 0..args.len() {
                        codes.push(Statement::Popq(Register::RCX));
                    }
                    // stack = .. .. .. .. ..
                    // push function return value
                    codes.push(Statement::Pushq(Register::RAX));
                    // stack = .. .. .. .. RV   (RV is return value)
                    codes
                }
                _ => panic!("unexpected node in code_expr. node={:?}", node),
            }
        }

        fn code_lvar(&self, node: &Node) -> Vec<Statement> {
            match node {
                Node::Variable(_, offset) => {
                    let abs_offset = offset.abs() as u64;
                    vec![
                        // FIXME: support Irmovq for negative integer
                        Statement::Irmovq(Imm::Integer(abs_offset), Register::RAX),
                        Statement::Rrmovq(Register::RBP, Register::RBX),
                        if *offset >= 0 {
                            Statement::Addq(Register::RAX, Register::RBX)
                        } else {
                            Statement::Subq(Register::RAX, Register::RBX)
                        },
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
        fn test_parser_expr() {
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
                // Token::Op(';'),
            ];
            let mut parser = Parser::new(tokens);
            let m_1 = num(1);
            let m_2_3 = mul(num(2), neg(num(3)));
            let m_4_5 = div(num(4), num(5));
            let expe = sub(add(m_1, m_2_3), m_4_5);
            // let expe = Prog{node: block(vec![expe])};
            let calc = parser.parse_expr();
            assert_eq!(expe, calc);
        }

        #[test]
        fn test_parse_stmt_if() {
            let tokens = vec![
                Token::Op('{'),
                Token::Int,
                Token::Id(String::from("a")),
                Token::Op(';'),
                Token::Int,
                Token::Id(String::from("b")),
                Token::Op(';'),
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
                Token::Op('}'),
            ];
            let mut parser = Parser::new(tokens);
            let if_stmt =             Box::new(Node::BinaryOp(
                BinaryOp::If,
                Box::new(Node::BinaryOp(
                    BinaryOp::Eq,
                    Box::new(Node::Variable(String::from("a"), -8)),
                    Box::new(Node::Num(2)),
                )),
                Box::new(Node::BinaryOp(
                    BinaryOp::Assign,
                    Box::new(Node::Variable(String::from("b"), -16)),
                    Box::new(Node::Num(1)),
                )),
            ));
            let expe = block(vec![
                Box::new(Node::DefVar(Type::Int, String::from("a"))),
                Box::new(Node::DefVar(Type::Int, String::from("b"))),
                if_stmt]);
            let calc = parser.parse_stmt();
            assert_eq!(expe, calc);
        }

        #[test]
        fn test_parse_stmt_block() {
            let tokens = vec![
                Token::Op('{'),
                Token::Int,
                Token::Id(String::from("xxb")),
                Token::Op(';'),
                Token::Int,
                Token::Id(String::from("abc")),
                Token::Op(';'),
                Token::If,
                Token::Op('('),
                Token::Id(String::from("xxb")),
                Token::Op(')'),
                Token::Op('{'),
                Token::Id(String::from("abc")),
                Token::Op('='),
                Token::Num(1),
                Token::Op(';'),
                Token::Num(4),
                Token::Op(';'),
                Token::Op('}'),
                Token::Op('}'),
            ];
            let mut parser = Parser::new(tokens);
            let if_stmt = Box::new(Node::BinaryOp(
                BinaryOp::If,
                Box::new(Node::Variable(String::from("xxb"), -8)),
                Box::new(Node::Block(vec![
                    Box::new(Node::BinaryOp(
                        BinaryOp::Assign,
                        Box::new(Node::Variable(String::from("abc"), -16)),
                        Box::new(Node::Num(1)),
                    )),
                    Box::new(Node::Num(4)),
                ])),
            ));
            let expe = block(vec![
                Box::new(Node::DefVar(Type::Int, String::from("xxb"))),
                Box::new(Node::DefVar(Type::Int, String::from("abc"))),
                if_stmt]);
            let calc = parser.parse_stmt();
            assert_eq!(expe, calc);
        }

        #[test]
        fn test_parse_def() {
            let tokens = vec![
                Token::Id(String::from("f")),
                Token::Op('('),
                Token::Id(String::from("a")),
                Token::Op(','),
                Token::Id(String::from("b")),
                Token::Op(')'),
                Token::Op('{'),
                Token::Int,
                Token::Id(String::from("c")),
                Token::Op(';'),
                Token::Int,
                Token::Id(String::from("d")),
                Token::Op(';'),                                
                Token::Id(String::from("c")),
                Token::Op('='),
                Token::Id(String::from("a")),
                Token::Op('+'),
                Token::Id(String::from("b")),
                Token::Op(';'),
                Token::Id(String::from("d")),
                Token::Op('='),
                Token::Id(String::from("c")),
                Token::Op(';'),
                Token::Op('}'),
            ];
            let mut parser = Parser::new(tokens);
            let var_a = var("a", 8 + 8);
            let var_b = var("b", 8 + 16);
            let var_c = var("c", -8);
            let var_c2 = var("c", -8);
            let var_d = var("d", -16);
            let expe = Prog {
                node: block(vec![Box::new(Node::DefFun(
                    String::from("f"),
                    vec![String::from("a"), String::from("b")],
                    block(vec![
                        Box::new(Node::DefVar(Type::Int, String::from("c"))),
                        Box::new(Node::DefVar(Type::Int, String::from("d"))),
                        assign(var_c, add(var_a, var_b)),
                        assign(var_d, var_c2),
                    ]),
                    2,
                ))]),
            };
            let calc = parser.parse_prog();
            assert_eq!(expe, calc);
        }

        #[test]
        fn test_code() {
            let coder = Coder {};
            // a = 1 + 2
            let a = add(num(23), num(34));
            let p = Prog {
                node: block(vec![a]),
            };
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
                Statement::Popq(Register::RAX),
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

        fn assign(left: Box<Node>, right: Box<Node>) -> Box<Node> {
            Box::new(Node::BinaryOp(BinaryOp::Assign, left, right))
        }

        fn var(name: &str, offset: i64) -> Box<Node> {
            Box::new(Node::Variable(name.to_string(), offset))
        }

        fn block(stmts: Vec<Box<Node>>) -> Box<Node> {
            Box::new(Node::Block(stmts))
        }
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
