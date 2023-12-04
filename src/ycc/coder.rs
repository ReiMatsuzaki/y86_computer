use super::{node::*, INIT_SP};

use crate::yas::{Dest, Imm, ModDest, Register, Code};

pub struct Coder {
    label_count: u64,
}

impl Coder {
    pub fn new() -> Self {
        Coder { label_count: 0 }
    }

    pub fn code(&mut self, prog: &Prog) -> Vec<Code> {
        let mut stmts = self.code_prologue();
        let node = prog.get_node();
        stmts.append(&mut self.code_stmt(&node));
        stmts
    }

    fn code_prologue(&self) -> Vec<Code> {
        let mut codes = vec![
            // initialize stack pointer and meaning less base pointer
            Code::Irmovq(Imm::Integer(INIT_SP), Register::RSP),
            Code::Irmovq(Imm::Integer(INIT_SP + 10), Register::RBP),
        ];
        let n = Box::new(Node::Call("main".to_string(), vec![]));
        codes.append(&mut self.code_expr(&n));
        codes.append(&mut vec![Code::Halt]);
        codes
    }

    fn produce_label(&mut self, base: &str) -> String {
        let label = format!("{0}_{1}", base, self.label_count);
        self.label_count += 1;
        label
    }

    fn code_stmt(&mut self, node: &Node) -> Vec<Code> {
        // parse node as stmt. number of stack is presevered. exception is ret.
        match node {
            Node::DefFun(name, block, lvars_size) => {
                let mut codes = vec![
                    // stack = .. .. .. .. RE (RE is return addressed)
                    // jump label
                    Code::Label(name.to_string()),
                    // save previous registers
                    Code::Pushq(Register::RBP),
                    // stack = .. .. .. OB RE (OB is old base pointer)
                    // set new base pointer
                    Code::Rrmovq(Register::RSP, Register::RBP),
                    // allocate local variables  (deallocating is done in ret)
                    Code::Irmovq(Imm::Integer(*lvars_size as u64), Register::RAX),
                    Code::Subq(Register::RAX, Register::RSP),
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
            Node::If(cond, then) => {
                let false_label = self.produce_label("iffalse");
                let mut codes = vec![];
                codes.append(&mut self.code_expr(cond));
                codes.append(&mut vec![
                    Code::Popq(Register::RAX),
                    Code::Irmovq(Imm::Integer(0), Register::RBX),
                    Code::Subq(Register::RBX, Register::RAX),
                    Code::Je(Dest::Label(false_label.to_string())),
                ]);
                codes.append(&mut self.code_stmt(then));
                codes.append(&mut vec![Code::Label(false_label.to_string())]);
                codes
            }
            Node::While(cond, then) => {
                let begin_label = self.produce_label("whilebegin");
                let end_label = self.produce_label("whileend");
                let mut codes = vec![];
                codes.append(&mut vec![Code::Label(begin_label.to_string())]);
                codes.append(&mut self.code_expr(cond));
                codes.append(&mut vec![
                    Code::Popq(Register::RAX),
                    Code::Irmovq(Imm::Integer(0), Register::RBX),
                    Code::Addq(Register::RBX, Register::RAX),
                    Code::Je(Dest::Label(end_label.to_string())),
                ]);
                codes.append(&mut self.code_stmt(then));
                codes.append(&mut vec![
                    Code::Jmp(Dest::Label(begin_label.to_string())),
                    Code::Label(end_label.to_string()),
                ]);
                codes
            }
            Node::Ret(node) => {
                let mut codes = vec![];
                codes.append(&mut self.code_expr(node));
                codes.append(&mut vec![
                    // stack = RV L2 L1 OB RE (RV=resultant value. OB is old base pointer. RE is return address)
                    // remove current result
                    Code::Popq(Register::RAX),
                    // stack = .. L2 L1 OB RE
                    // deallocate local variables
                    Code::Rrmovq(Register::RBP, Register::RSP),
                    // stack = .. .. .. OB RE
                    // restore old register
                    Code::Popq(Register::RBP),
                    // stack = .. .. .. .. RE
                    Code::Ret,
                ]);
                codes
            }
            _ => {
                let mut codes = self.code_expr(node);
                codes.push(Code::Popq(Register::RAX));
                codes
            }
        }
    }

    fn is_pointer(node: &Node) -> bool {
        match node {
            Node::Variable(Type::Ptr, _) => true,
            _ => false,
        }
    }

    fn code_lincomb(&self, x: u64, y: u64, plus_minus: char) -> Vec<Code> {
        // calculate x * %rax + y * %rbx and store to %rax
        // assume %rax and %rbx are
        let mut xs = vec![];
        if x != 1 {
            xs.append(&mut vec![
                Code::Irmovq(Imm::Integer(x), Register::RCX),
                Code::Mulq(Register::RCX, Register::RAX),
            ]);
        }
        if y != 1 {
            xs.append(&mut vec![
                Code::Irmovq(Imm::Integer(y), Register::RCX),
                Code::Mulq(Register::RCX, Register::RBX),
            ]);
        }
        match plus_minus {
            '+' => xs.append(&mut vec![Code::Addq(Register::RBX, Register::RAX)]),
            '-' => xs.append(&mut vec![Code::Subq(Register::RBX, Register::RAX)]),
            _ => panic!("unexpected plus_minus"),
        }
        xs
    }

    fn code_expr(&self, node: &Node) -> Vec<Code> {
        // parse node as expr and remain result at stack top
        match node {
            Node::BinaryOp(BinaryOp::Assign, left, right) => {
                let mut codes = self.code_lvar(left);
                codes.append(&mut self.code_expr(right));
                codes.append(&mut vec![
                    Code::Popq(Register::RBX),
                    Code::Popq(Register::RAX), // (%rax) = %rbx
                    Code::Rmmovq(
                        Register::RBX,
                        ModDest {
                            dest: crate::yas::Dest::Integer(0),
                            register: Register::RAX,
                        },
                    ),
                    Code::Pushq(Register::RBX),
                ]);
                codes
            }
            Node::BinaryOp(op, left, right) => {
                let mut codes = self.code_expr(left);
                codes.append(&mut self.code_expr(right));
                codes.push(Code::Popq(Register::RBX));
                codes.push(Code::Popq(Register::RAX)); // %rax OP %rbx
                let mut ss = match op {
                    BinaryOp::Add => {
                        let x = if Self::is_pointer(right) { 8 } else { 1 };
                        let y = if Self::is_pointer(left) { 8 } else { 1 };
                        self.code_lincomb(x, y, '+')
                    }
                    BinaryOp::Sub => {
                        let x = if Self::is_pointer(right) { 8 } else { 1 };
                        let y = if Self::is_pointer(left) { 8 } else { 1 };
                        self.code_lincomb(x, y, '-')
                    }
                    BinaryOp::Mul => vec![Code::Mulq(Register::RBX, Register::RAX)],
                    BinaryOp::Div => vec![Code::Divq(Register::RBX, Register::RAX)],
                    BinaryOp::Eq => vec![
                        Code::Irmovq(Imm::Integer(0), Register::RSI),
                        Code::Irmovq(Imm::Integer(1), Register::RDI),
                        Code::Subq(Register::RBX, Register::RAX),
                        Code::Cmove(Register::RDI, Register::RSI),
                        Code::Rrmovq(Register::RSI, Register::RAX),
                    ],
                    BinaryOp::Less => vec![
                        Code::Irmovq(Imm::Integer(0), Register::RSI),
                        Code::Irmovq(Imm::Integer(1), Register::RDI),
                        Code::Subq(Register::RBX, Register::RAX),
                        Code::Cmovl(Register::RDI, Register::RSI),
                        Code::Rrmovq(Register::RSI, Register::RAX),
                    ],
                    _ => panic!("unexpected binary op"),
                };
                codes.append(&mut ss);
                codes.push(Code::Pushq(Register::RAX));
                codes
            }
            Node::UnaryOp(UnaryOp::Neg, unode) => {
                // replace -x => 0 - x
                let mut codes = self.code_expr(unode);
                let mut ss = vec![
                    Code::Popq(Register::RBX),
                    Code::Irmovq(Imm::Integer(0), Register::RAX),
                    Code::Subq(Register::RBX, Register::RAX), // 0 sub %rbx
                    Code::Pushq(Register::RAX),
                ];
                codes.append(&mut ss);
                codes
            }
            Node::Num(n) => {
                vec![
                    Code::Irmovq(Imm::Integer(*n), Register::RBX),
                    Code::Pushq(Register::RBX),
                ]
            }
            Node::Variable(_, _) | Node::AryElem(_, _, _) => {
                let mut codes = self.code_lvar(node);
                codes.push(Code::Popq(Register::RAX));
                codes.push(Code::Mrmovq(
                    ModDest {
                        dest: crate::yas::Dest::Integer(0),
                        register: Register::RAX,
                    },
                    Register::RAX,
                ));
                codes.push(Code::Pushq(Register::RAX));
                codes
            }
            Node::UnaryOp(UnaryOp::Deref, expr) => {
                // assume expr is Variable and push its value (address of something)
                let mut codes = self.code_expr(expr);
                codes.push(Code::Popq(Register::RAX));
                codes.push(Code::Mrmovq(
                    ModDest {
                        dest: crate::yas::Dest::Integer(0),
                        register: Register::RAX,
                    },
                    Register::RAX,
                ));
                codes.push(Code::Pushq(Register::RAX));
                codes
            }
            Node::UnaryOp(UnaryOp::Addr, expr) => {
                // assume expr is Variable and push its address
                self.code_lvar(expr)
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
                codes.push(Code::Call(Dest::Label(name.to_string())));
                // stack = .. .. .. a1 a2
                // pop args on stack
                for _ in 0..args.len() {
                    codes.push(Code::Popq(Register::RCX));
                }
                // stack = .. .. .. .. ..
                // push function return value
                codes.push(Code::Pushq(Register::RAX));
                // stack = .. .. .. .. RV   (RV is return value)
                codes
            }
            _ => panic!("unexpected node in code_expr. node={:?}", node),
        }
    }

    fn code_lvar(&self, node: &Node) -> Vec<Code> {
        match node {
            Node::UnaryOp(UnaryOp::Deref, expr) => {
                // assume expr is Variable and push its value (address of something)
                self.code_expr(expr)
            }
            Node::Variable(_, offset) => {
                let abs_offset = offset.abs() as u64;
                vec![
                    // FIXME: support Irmovq for negative integer
                    Code::Irmovq(Imm::Integer(abs_offset), Register::RAX),
                    Code::Rrmovq(Register::RBP, Register::RBX),
                    if *offset >= 0 {
                        Code::Addq(Register::RAX, Register::RBX)
                    } else {
                        Code::Subq(Register::RAX, Register::RBX)
                    },
                    Code::Pushq(Register::RBX),
                ]
            }
            Node::AryElem(ty, offset, index) => {
                // FIXME: impl as a[index] -> *(a + index)
                let mut codes = self.code_expr(index);
                let var = Box::new(Node::Variable(ty.clone(), *offset));
                codes.append(&mut self.code_lvar(&var));
                codes.append(&mut vec![
                    Code::Popq(Register::RAX), // %rax: address of a[0]
                    Code::Popq(Register::RBX), // %rbx: index
                    Code::Irmovq(Imm::Integer(ty.size() as u64), Register::RCX),
                    Code::Mulq(Register::RCX, Register::RBX),
                    Code::Subq(Register::RBX, Register::RAX),
                    Code::Pushq(Register::RAX),
                ]);
                codes
            }
            _ => panic!("unexpected node in code_lvar"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::node::test_utils::*;
    use super::*;
    use crate::yas::{Imm, Register, Code};

    #[test]
    fn test_code() {
        let mut coder = Coder::new();
        // a = 1 + 2
        let a = add(num(23), num(34));
        let p = Prog::new(block(vec![a]));
        let mut expe = coder.code_prologue();
        expe.append(&mut vec![
            Code::Irmovq(Imm::Integer(23), Register::RBX),
            Code::Pushq(Register::RBX),
            Code::Irmovq(Imm::Integer(34), Register::RBX),
            Code::Pushq(Register::RBX),
            Code::Popq(Register::RBX),
            Code::Popq(Register::RAX),
            Code::Addq(Register::RBX, Register::RAX),
            Code::Pushq(Register::RAX),
            Code::Popq(Register::RAX),
        ]);
        let calc = coder.code(&p);
        assert_eq!(expe, calc);
    }
}
