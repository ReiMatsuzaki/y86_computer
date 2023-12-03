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
        Prog::new(node)
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
                    Box::new(Node::DefVar)
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
            Box::new(Node::DefVar),
            Box::new(Node::DefVar),
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
            Box::new(Node::DefVar),
            Box::new(Node::DefVar),
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
        let expe = Prog::new(
            block(vec![Box::new(Node::DefFun(
                String::from("f"),
                vec![String::from("a"), String::from("b")],
                block(vec![
                    Box::new(Node::DefVar),
                    Box::new(Node::DefVar),
                    assign(var_c, add(var_a, var_b)),
                    assign(var_d, var_c2),
                ]),
                2,
            ))]),
        );
        let calc = parser.parse_prog();
        assert_eq!(expe, calc);
    }

}