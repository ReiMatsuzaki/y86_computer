use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Register {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    RNONE,
}

impl FromStr for Register {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "%rax" => Ok(Register::RAX),
            "%rcx" => Ok(Register::RCX),
            "%rbx" => Ok(Register::RBX),
            "%rdx" => Ok(Register::RDX),
            "%rsp" => Ok(Register::RSP),
            "%rbp" => Ok(Register::RBP),
            "%rsi" => Ok(Register::RSI),
            "%rdi" => Ok(Register::RDI),
            "%r8" => Ok(Register::R8),
            "%r9" => Ok(Register::R9),
            "%r10" => Ok(Register::R10),
            "%r11" => Ok(Register::R11),
            _ => Err(format!("Invalid register: {}", input)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Value(u64),
    Label(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Directive {
    Pos(u64),
    Quad(Expr)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Code {
    Directive(Directive),
    Label(String),
    Halt,
    Nop,
    Rrmovq(Register, Register),
    Irmovq(Register, Expr),
    Rmmovq(Register, Register, Expr),
    Mrmovq(Register, Register, Expr),
    Addq(Register, Register),
    Subq(Register, Register),
    Andq(Register, Register),
    Orq(Register, Register),
    Mulq(Register, Register),
    Divq(Register, Register),
    Jmp(Expr),
    // Jle(Dest),
    // Jl(Dest),
    Je(Expr),
    Jne(Expr),
    Call(Expr),
    Cmovl(Register, Register),
    Cmove(Register, Register),
    Cmovne(Register, Register),
    Ret,
    Pushq(Register),
    Popq(Register),
}
