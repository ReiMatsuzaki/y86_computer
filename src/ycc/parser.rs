// q: change function result type to Result<T, E>
// a:

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

#[derive(Debug)]
pub struct ParserError {
    pub token: Token,
    pub pos: usize,
    pub message: String,
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

    pub fn parse(&mut self) -> Result<Prog, ParserError> {
        self.parse_prog()
    }

    fn error<T>(&self, msg: &str) -> Result<T, ParserError> {
        Err(ParserError {
            token: self.tokens[self.pos].clone(),
            pos: self.pos,
            message: msg.to_string(),
        })
    }

    fn expect(&mut self, token: &Token) -> Result<(), ParserError> {
        if self.tokens[self.pos] == *token {
            self.pos += 1;
            Ok(())
        } else {
            self.error("unexpected token.")
        }
    }

    fn expect_type(&mut self) -> Result<Type, ParserError> {
        // FIXME: return Result type
        match self.tokens[self.pos] {
            Token::Int => {
                self.pos += 1;
                if let Some(Token::Op('*')) = self.tokens.get(self.pos) {
                    self.pos += 1;
                    Ok(Type::Ptr)
                } else {
                    Ok(Type::Int)
                }
            }
            _ => self.error("type is expected"),
        }
    }

    fn expect_id(&mut self) -> Result<String, ParserError> {
        match self.tokens[self.pos] {
            Token::Id(ref s) => {
                self.pos += 1;
                Ok(s.to_string())
            }
            _ => self.error("Ident is expected"),
        }
    }

    fn expect_num(&mut self) -> Result<u64, ParserError> {
        match self.tokens[self.pos] {
            Token::Num(i) => {
                self.pos += 1;
                Ok(i)
            }
            _ => self.error("Num is expected"),
        }
    }

    fn parse_prog(&mut self) -> Result<Prog, ParserError> {
        let mut stmts = vec![];
        while self.pos < self.tokens.len() {
            stmts.push(self.parse_deffun()?);
        }
        let node = Box::new(Node::Block(stmts));
        Ok(Prog::new(node))
    }

    fn parse_deffun(&mut self) -> Result<Box<Node>, ParserError> {
        self.expect(&Token::Int)?;
        // FIXME: refactor using self.expect_id()
        match self.tokens.get(self.pos) {
            Some(Token::Id(id)) => {
                let name = String::from(id);
                self.pos += 1;
                self.parse_argvar()?;
                self.expect(&Token::Op('{'))?;
                self.parse_defvar()?;
                let mut block = vec![];
                while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                    match token {
                        Token::Op('}') => {
                            self.pos += 1;
                            break;
                        }
                        _ => {
                            let n = self.parse_stmt();
                            block.push(n?);
                        }
                    }
                }
                let block = Box::new(Node::Block(block));
                let lvar_bytes = self.lvars.iter().map(|v| v.ty.size()).sum();
                Ok(Box::new(Node::DefFun(name, block, lvar_bytes)))
            }
            // FIXME: remove panic
            Some(_) => panic!("invalid token, pos={0}", self.pos),
            None => panic!("token not found. pos={}", self.pos),
        }
    }

    fn parse_argvar(&mut self) -> Result<(), ParserError> {
        self.expect(&Token::Op('('))?;
        self.args = vec![];
        while let Some(token) = self.tokens.get(self.pos) {
            if Token::Op(')') == *token {
                self.pos += 1;
                break;
            }
            let ty = self.expect_type()?;
            let name = self.expect_id()?;
            let offset = 16 + self.args.iter().map(|v| v.ty.size() as i64).sum::<i64>();
            self.args.push(Variable { name, offset, ty });
            match self.tokens.get(self.pos) {
                Some(Token::Op(')')) => {
                    self.pos += 1;
                    break;
                }
                Some(Token::Op(',')) => {
                    self.pos += 1;
                }
                _ => return self.error("unexpected token in parse_argvar"),
            }
        }
        Ok(())
    }

    fn parse_defvar(&mut self) -> Result<(), ParserError> {
        self.lvars = vec![];
        while let Ok(ty) = self.expect_type() {
            let id = self.expect_id()?;
            match self.tokens.get(self.pos) {
                Some(Token::Op(';')) => {
                    self.pos += 1;
                    self.add_lvars(id.to_string(), ty)?;
                }
                Some(Token::Op('[')) => {
                    self.pos += 1;
                    // FIXME: refactor self.expect_num()
                    let n = self.expect_num()?;
                    self.expect(&Token::Op(']'))?;
                    self.expect(&Token::Op(';'))?;
                    let ty = Type::Ary(Box::new(ty), n as usize);
                    self.add_lvars(id.to_string(), ty)?;
                }
                _ => return self.error("unexpected token in defvar."),
            }
        }
        Ok(())
    }

    fn add_lvars(&mut self, id: String, ty: Type) -> Result<(), ParserError> {
        if self.lvars.iter().any(|v| v.name.eq(&id)) {
            return self.error("variable already defined");
        }
        let offset = -8 - self.lvars.iter().map(|v| v.ty.size() as i64).sum::<i64>();
        let v = Variable {
            name: id.to_string(),
            offset,
            ty: ty.clone(),
        };
        self.lvars.push(v);
        Ok(())
    }

    fn parse_stmt(&mut self) -> Result<Box<Node>, ParserError> {
        match self.tokens.get(self.pos) {
            Some(Token::Op('{')) => {
                self.pos += 1;
                let stmt = self.parse_stmt()?;
                let mut stmts = vec![stmt];
                while let Some(&token) = self.tokens.get(self.pos).as_ref() {
                    match token {
                        Token::Op('}') => {
                            self.pos += 1;
                            break;
                        }
                        _ => stmts.push(self.parse_stmt()?),
                    }
                }
                Ok(Box::new(Node::Block(stmts)))
            }
            Some(Token::Return) => {
                self.pos += 1;
                let expr = self.parse_expr()?;
                self.expect(&Token::Op(';'))?;
                Ok(Box::new(Node::Ret(expr)))
            }
            Some(Token::While) => {
                self.pos += 1;
                self.expect(&Token::Op('('))?;
                let expr = self.parse_expr()?;
                self.expect(&Token::Op(')'))?;
                let stmt = self.parse_stmt()?;
                Ok(Box::new(Node::While(expr, stmt)))
            }
            Some(Token::If) => {
                self.pos += 1;
                self.expect(&Token::Op('('))?;
                let expr = self.parse_expr()?;
                self.expect(&Token::Op(')'))?;
                let stmt = self.parse_stmt()?;
                Ok(Box::new(Node::If(expr, stmt)))
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(&Token::Op(';'))?;
                Ok(expr)
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Box<Node>, ParserError> {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> Result<Box<Node>, ParserError> {
        let left = self.parse_rel()?;
        if let Some(&Token::Op('=')) = self.tokens.get(self.pos) {
            self.pos += 1;
            let right = self.parse_rel()?;
            Ok(Box::new(Node::BinaryOp(BinaryOp::Assign, left, right)))
        } else {
            Ok(left)
        }
    }

    fn parse_rel(&mut self) -> Result<Box<Node>, ParserError> {
        let mut left = self.parse_add()?;
        while let Some(&token) = self.tokens.get(self.pos).as_ref() {
            match token {
                Token::Op2(['=', '=']) => {
                    self.pos += 1;
                    let right = self.parse_add()?;
                    left = Box::new(Node::BinaryOp(BinaryOp::Eq, left, right));
                }
                Token::Op('<') => {
                    self.pos += 1;
                    let right = self.parse_add()?;
                    left = Box::new(Node::BinaryOp(BinaryOp::Less, left, right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_add(&mut self) -> Result<Box<Node>, ParserError> {
        let mut left = self.parse_mul()?;
        while let Some(&token) = self.tokens.get(self.pos).as_ref() {
            match token {
                Token::Op('+') => {
                    self.pos += 1;
                    let right = self.parse_mul()?;
                    left = Box::new(Node::BinaryOp(BinaryOp::Add, left, right));
                }
                Token::Op('-') => {
                    self.pos += 1;
                    let right = self.parse_mul()?;
                    left = Box::new(Node::BinaryOp(BinaryOp::Sub, left, right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_mul(&mut self) -> Result<Box<Node>, ParserError> {
        let mut left = self.parse_unary()?;
        while let Some(&token) = self.tokens.get(self.pos).as_ref() {
            match token {
                Token::Op('*') => {
                    self.pos += 1;
                    let right = self.parse_unary()?;
                    left = Box::new(Node::BinaryOp(BinaryOp::Mul, left, right));
                }
                Token::Op('/') => {
                    self.pos += 1;
                    let right = self.parse_unary()?;
                    left = Box::new(Node::BinaryOp(BinaryOp::Div, left, right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Box<Node>, ParserError> {
        match self.tokens.get(self.pos) {
            Some(Token::Op('-')) => {
                self.pos += 1;
                let primary = self.parse_primary()?;
                Ok(Box::new(Node::UnaryOp(UnaryOp::Neg, primary)))
            }
            Some(Token::Op('*')) => {
                self.pos += 1;
                let primary = self.parse_primary()?;
                Ok(Box::new(Node::UnaryOp(UnaryOp::Deref, primary)))
            }
            Some(Token::Op('&')) => {
                self.pos += 1;
                let primary = self.parse_primary()?;
                Ok(Box::new(Node::UnaryOp(UnaryOp::Addr, primary)))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Box<Node>, ParserError> {
        match self.tokens.get(self.pos) {
            Some(Token::Num(n)) => {
                self.pos += 1;
                Ok(Box::new(Node::Num(*n)))
            }
            Some(Token::Id(id)) => {
                self.pos += 1;
                let name = String::from(id);
                match self.tokens.get(self.pos) {
                    Some(Token::Op('[')) => {
                        // array access
                        self.pos += 1;
                        let v = *self.find_var(&id)?;
                        let (ty, offset) = match v {
                            Node::Variable(Type::Ary(ty, _), offset) => (*ty, offset),
                            _ => {
                                return self.error("unexpected type")
                            }
                        };
                        let index = self.parse_expr()?;
                        self.expect(&Token::Op(']'))?;
                        Ok(Box::new(Node::AryElem(ty, offset, index)))
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
                                _ => args.push(self.parse_expr()?),
                            }
                        }
                        Ok(Box::new(Node::Call(name, args)))
                    }
                    _ => self.find_var(&id),
                }
            }
            Some(Token::Op('(')) => {
                self.pos += 1;
                let expr = self.parse_expr()?;
                self.expect(&Token::Op(')'))?;
                Ok(expr)
            }
            _ => self.error("unexpected token as primary."),
        }
    }

    fn find_var(&self, id: &str) -> Result<Box<Node>, ParserError> {
        // variable
        //                      +-(RBP)
        // stack = .. L2 L1 L0 OB RE A0 A1 A2 ..
        let argi = self.args.iter().find(|x| x.name == id);
        let lvari = self.lvars.iter().find(|x| x.name == id);
        let v = match (argi, lvari) {
            (Some(i), None) => i,
            (None, Some(i)) => i,
            _ => return self.error("lvar not found."),
        };
        Ok(Box::new(Node::Variable(v.ty.clone(), v.offset)))
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
        let calc = parser.parse_expr().unwrap();
        assert_eq!(expe, calc);
    }

    #[test]
    fn test_parse_stmt_if() {
        let tokens = vec![
            Token::Int,
            Token::Id(String::from("f")),
            Token::Op('('),
            Token::Op(')'),
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
            // Box::new(Node::DefVar),
            // Box::new(Node::DefVar),
            if_stmt,
        ]);
        let expe = Box::new(Node::DefFun("f".to_string(), expe, 16));
        let calc = parser.parse_deffun().unwrap();
        assert_eq!(expe, calc);
    }

    #[test]
    fn test_parse_stmt_block() {
        let tokens = vec![
            Token::Int,
            Token::Id(String::from("f")),
            Token::Op('('),
            Token::Op(')'),
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
        let b = block(vec![if_stmt]);
        let expe = Box::new(Node::DefFun("f".to_string(), b, 16));
        let calc = parser.parse_deffun().unwrap();
        assert_eq!(expe, calc);
    }

    #[test]
    fn test_parse_def() {
        let tokens = vec![
            Token::Int,
            Token::Id(String::from("f")),
            Token::Op('('),
            Token::Int,
            Token::Id(String::from("a")),
            Token::Op(','),
            Token::Int,
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
                assign(var_c, add(addr(var_a), deref(var_b))),
                assign(var_d, var_c2),
            ]),
            16,
        ))]));
        let calc = parser.parse_prog().unwrap();
        assert_eq!(expe, calc);
    }

    // FIXME: refactor test name to array_test
    #[test]
    fn test_array() {
        let tokens = vec![
            Token::Int,
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
            block(vec![aryelm]),
            8 * 10,
        ))]));
        let calc = parser.parse_prog().unwrap();
        assert_eq!(expe, calc);
    }
}
