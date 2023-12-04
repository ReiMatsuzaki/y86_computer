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
use super::node::*;
use super::token::Token;

pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pos: usize,
    args: Vec<Variable>,
    lvars: Vec<Variable>,
}

#[derive(Debug, PartialEq, Clone)]
struct Variable {
    name: String,
    offset: i64,
    ty: Type,
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

    fn build_arg_vars(args: &Vec<String>) -> Vec<Variable> {
        let mut vars = vec![];
        for (i, arg) in args.iter().enumerate() {
            vars.push(Variable {
                name: arg.to_string(),
                offset: 8 * (2 + i as i64),
                ty: Type::Int,
            });
        }
        vars
    }

    fn add_lvars(&mut self, id: String, ty: Type) {
        if self.lvars.iter().any(|v| v.name.eq(&id)) {
            panic!("variable already defined");
        }
        let offset = -8-self.lvars.iter().map(|v| v.ty.size() as i64).sum::<i64>();
        let v = Variable {
            name: id.to_string(),
            offset,
            ty: ty.clone(),
        };
        self.lvars.push(v);
    }

    fn parse_prog(&mut self) -> Prog {
        let mut stmts = vec![];
        while self.pos < self.tokens.len() {
            stmts.push(self.parse_deffun());
        }
        let node = Box::new(Node::Block(stmts));
        Prog::new(node)
    }

    fn parse_list(&mut self) -> Vec<String> {
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
        args
    }

    fn parse_deffun(&mut self) -> Box<Node> {
        match self.tokens.get(self.pos) {
            Some(Token::Id(id)) => {
                let name = String::from(id);
                self.pos += 1;
                let args = self.parse_list();

                // init lvars and args. Pushed to lvars and refered in self.parse_stmt()
                self.lvars = vec![];
                self.args = Self::build_arg_vars(&args);
                let block = self.parse_stmt();
                let lvar_bytes = self.lvars.iter().map(|v| v.ty.size()).sum();
                Box::new(Node::DefFun(name, block, lvar_bytes))
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
                Box::new(Node::Ret(expr))
            }
            Some(Token::While) => {
                self.pos += 1;
                self.expect(&Token::Op('('));
                let expr = self.parse_expr();
                self.expect(&Token::Op(')'));
                let stmt = self.parse_stmt();
                Box::new(Node::While(expr, stmt))
            }
            Some(Token::If) => {
                self.pos += 1;
                self.expect(&Token::Op('('));
                let expr = self.parse_expr();
                self.expect(&Token::Op(')'));
                let stmt = self.parse_stmt();
                Box::new(Node::If(expr, stmt))
            }
            Some(Token::Int) => {
                self.pos += 1;
                let ty = if let Some(Token::Op('*')) = self.tokens.get(self.pos) {
                    self.pos += 1;
                    Type::Ptr
                } else {
                    Type::Int
                };
                if let Some(Token::Id(id)) = self.tokens.get(self.pos) {
                    self.pos += 1;
                    match self.tokens.get(self.pos) {
                        Some(Token::Op(';')) => {
                            self.pos += 1;
                            self.add_lvars(id.to_string(), ty);
                            Box::new(Node::DefVar)
                        }
                        Some(Token::Op('[')) => {
                            self.pos += 1;
                            // FIXME: refactor self.expect_num()
                            let n = match self.tokens[self.pos] {
                                Token::Num(n) => n,
                                _ => panic!("unexpected token in defvar. token={:?}", self.tokens[self.pos]),
                            };
                            self.pos += 1;
                            let id = id.to_string();
                            self.expect(&Token::Op(']'));
                            self.expect(&Token::Op(';'));
                            let ty = Type::Ary(Box::new(ty), n as usize);
                            self.add_lvars(id.to_string(), ty);
                            Box::new(Node::DefVar)
                        }
                        _ => panic!("unexpected token in defvar. token={:?}", self.tokens[self.pos]),
                    }
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
        let left = self.parse_rel();
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
            Some(Token::Op('*')) => {
                self.pos += 1;
                let primary = self.parse_primary();
                Box::new(Node::UnaryOp(UnaryOp::Deref, primary))
            }
            Some(Token::Op('&')) => {
                self.pos += 1;
                let primary = self.parse_primary();
                Box::new(Node::UnaryOp(UnaryOp::Addr, primary))
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
                match self.tokens.get(self.pos) {
                    Some(Token::Op('[')) => {
                        // array access
                        self.pos += 1;
                        let (ty, offset) = match *self.find_var(&id) {
                            Node::Variable(ty, offset) => {
                                match ty {
                                    Type::Ary(ty, _) => (*ty, offset),
                                    _ => panic!("unexpected type in array access"),
                                }
                            }
                            _ => panic!("unexpected node in array access"),
                        };
                        let index = self.parse_expr();
                        self.expect(&Token::Op(']'));
                        Box::new(Node::AryElem(ty, offset, index))
                    }
                    Some(Token::Op('(')) => {
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
                    }
                    _ => self.find_var(&id),
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
        let argi = self.args.iter().find(|x| x.name == id);
        let lvari = self.lvars.iter().find(|x| x.name == id);
        let v = match (argi, lvari) {
            (Some(i), None) => i,
            (None, Some(i)) => i,
            _ => panic!("lvar not found. name={}. pos={}", id, self.pos),
        };
        Box::new(Node::Variable(v.ty.clone(), v.offset))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ycc::node::test_utils::*;

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
        let if_stmt = Box::new(Node::If(
            Box::new(Node::BinaryOp(
                BinaryOp::Eq,
                Box::new(Node::Variable(Type::Int, -8)),
                Box::new(Node::Num(2)),
            )),
            Box::new(Node::BinaryOp(
                BinaryOp::Assign,
                Box::new(Node::Variable(Type::Int, -16)),
                Box::new(Node::Num(1)),
            )),
        ));
        let expe = block(vec![
            Box::new(Node::DefVar),
            Box::new(Node::DefVar),
            if_stmt,
        ]);
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
        let if_stmt = Box::new(Node::If(
            Box::new(Node::Variable(Type::Int, -8)),
            Box::new(Node::Block(vec![
                Box::new(Node::BinaryOp(
                    BinaryOp::Assign,
                    Box::new(Node::Variable(Type::Int, -16)),
                    Box::new(Node::Num(1)),
                )),
                Box::new(Node::Num(4)),
            ])),
        ));
        let expe = block(vec![
            Box::new(Node::DefVar),
            Box::new(Node::DefVar),
            if_stmt,
        ]);
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
            Token::Op('*'),
            Token::Id(String::from("d")),
            Token::Op(';'),
            Token::Id(String::from("c")),
            Token::Op('='),
            Token::Op('&'),
            Token::Id(String::from("a")),
            Token::Op('+'),
            Token::Op('*'),
            Token::Id(String::from("b")),
            Token::Op(';'),
            Token::Id(String::from("d")),
            Token::Op('='),
            Token::Id(String::from("c")),
            Token::Op(';'),
            Token::Op('}'),
        ];
        let mut parser = Parser::new(tokens);
        let var_a = var(Type::Int, 8 + 8);
        let var_b = var(Type::Int, 8 + 16);
        let var_c = var(Type::Int, -8);
        let var_c2 = var(Type::Int, -8);
        let var_d = var(Type::Ptr, -16);
        let expe = Prog::new(block(vec![Box::new(Node::DefFun(
            String::from("f"),
            block(vec![
                Box::new(Node::DefVar),
                Box::new(Node::DefVar),
                assign(var_c, add(addr(var_a), deref(var_b))),
                assign(var_d, var_c2),
            ]),
            16,
        ))]));
        let calc = parser.parse_prog();
        assert_eq!(expe, calc);
    }

    // FIXME: refactor test name to array_test
    #[test]
    fn test_array() {
        let tokens = vec![
            Token::Id(String::from("f")),
            Token::Op('('),
            Token::Op(')'),
            Token::Op('{'),
            Token::Int,
            Token::Id(String::from("xs")),
            Token::Op('['),
            Token::Num(10),
            Token::Op(']'),
            Token::Op(';'),
            Token::Id(String::from("xs")),
            Token::Op('['),
            Token::Num(3),
            Token::Op(']'),
            Token::Op(';'),
            Token::Op('}'),
        ];
        let mut parser = Parser::new(tokens);
        let aryelm = Box::new(Node::AryElem(Type::Int, -8, num(3)));
        let expe = Prog::new(block(vec![Box::new(Node::DefFun(
            String::from("f"),
            block(vec![Box::new(Node::DefVar), aryelm]),
            8 * 10,
        ))]));
        let calc = parser.parse_prog();
        assert_eq!(expe, calc);
    }
}
