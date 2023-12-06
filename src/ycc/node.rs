#[derive(Debug, PartialEq)]
pub struct Prog {
    node: Box<Node>,
    global_vars: Vec<GlobalVar>,
}

impl Prog {
    pub fn new(node: Box<Node>, global_vars: Vec<GlobalVar>) -> Self {
        Prog { node, global_vars }
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

#[derive(Debug, PartialEq, Clone)]
pub struct GlobalVar {
    pub name: String,
    pub ty: Type,
    pub label: String,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    // Expr
    BinaryOp(BinaryOp, Box<Node>, Box<Node>),
    UnaryOp(UnaryOp, Box<Node>),
    Num(u64),
    // FIXME: define variable type
    // FIXME: String can be removed
    Variable(Type, i64), // type, offset; variable address is (offset + %RBP)
    AryElem(Type, i64, Box<Node>), // element type, offset, index
    
    // Stmt
    Block(Vec<Box<Node>>),
    If(Box<Node>, Box<Node>), // cond, then
    While(Box<Node>, Box<Node>), // cond, body
    DefFun(String, Box<Node>, usize), // name, block, lvars_bytes
    Call(String, Vec<Box<Node>>),     // name, args(each Node is Expr)
    Ret(Box<Node>), // return expr
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
    Deref,
    Addr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Ptr,
    Ary(Box<Type>, usize),
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Int => 8,
            Type::Ptr => 8,
            Type::Ary(t, n) => t.size() * n,
        }
    }

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

    pub fn var(t: Type, offset: i64) -> Box<Node> {
        Box::new(Node::Variable(t, offset))
    }

    pub fn block(stmts: Vec<Box<Node>>) -> Box<Node> {
        Box::new(Node::Block(stmts))
    }

    pub fn deref(expr: Box<Node>) -> Box<Node> {
        Box::new(Node::UnaryOp(UnaryOp::Deref, expr))
    }

    pub fn addr(expr: Box<Node>) -> Box<Node> {
        Box::new(Node::UnaryOp(UnaryOp::Addr, expr))
    }
}