#[derive(Debug, PartialEq)]
pub struct Prog {
    node: Box<Node>,
}

impl Prog {
    pub fn new(node: Box<Node>) -> Self {
        Prog { node }
    }

    pub fn get_node(&self) -> &Box<Node> {
        &self.node
    }

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
pub enum Node {
    // Expr
    BinaryOp(BinaryOp, Box<Node>, Box<Node>),
    UnaryOp(UnaryOp, Box<Node>),
    Num(u64),
    // FIXME: define variable type
    // FIXME: String can be removed
    Variable(String, i64), // name, offset; variable address is (offset + %RBP)
    
    // Stmt
    Block(Vec<Box<Node>>),
    If(Box<Node>, Box<Node>), // cond, then
    While(Box<Node>, Box<Node>), // cond, body
    DefVar,
    DefFun(String, Vec<String>, Box<Node>, usize), // name, args(String), block, num_lvar
    Call(String, Vec<Box<Node>>),                  // name, args(each Node is Expr)
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Assign,
    Eq,
    Less,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Ret,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
}

#[cfg(test)]
pub mod test_utils {
    use super::*;

    pub fn num(n: u64) -> Box<Node> {
        Box::new(Node::Num(n))
    }

    pub fn neg(node: Box<Node>) -> Box<Node> {
        Box::new(Node::UnaryOp(UnaryOp::Neg, node))
    }

    pub fn mul(left: Box<Node>, right: Box<Node>) -> Box<Node> {
        Box::new(Node::BinaryOp(BinaryOp::Mul, left, right))
    }

    pub fn div(left: Box<Node>, right: Box<Node>) -> Box<Node> {
        Box::new(Node::BinaryOp(BinaryOp::Div, left, right))
    }

    pub fn add(left: Box<Node>, right: Box<Node>) -> Box<Node> {
        Box::new(Node::BinaryOp(BinaryOp::Add, left, right))
    }

    pub fn sub(left: Box<Node>, right: Box<Node>) -> Box<Node> {
        Box::new(Node::BinaryOp(BinaryOp::Sub, left, right))
    }

    // fn eq(left: Box<Node>, right: Box<Node>) -> Box<Node> {
    //     Box::new(Node::BinaryOp(BinaryOp::Eq, left, right))
    // }

    pub fn assign(left: Box<Node>, right: Box<Node>) -> Box<Node> {
        Box::new(Node::BinaryOp(BinaryOp::Assign, left, right))
    }

    pub fn var(name: &str, offset: i64) -> Box<Node> {
        Box::new(Node::Variable(name.to_string(), offset))
    }

    pub fn block(stmts: Vec<Box<Node>>) -> Box<Node> {
        Box::new(Node::Block(stmts))
    }
}