#[derive(Debug, PartialEq, Clone)]
pub enum Y8R {
    RAX = 0x0,
    RCX = 0x1,
    RDX = 0x2,
    RBX = 0x3,
    RSP = 0x4,
    RBP = 0x5,
    RSI = 0x6,
    RDI = 0x7,
    R8 = 0x8,
    R9 = 0x9,
    R10 = 0xA,
    R11 = 0xB,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Y8S {
    AOK = 0x1,
    HLT = 0x2,
    // ADR = 0x3,
    INS = 0x4,
}

#[derive(Debug, Clone, Copy)]
pub enum OpqFn {
    ADD,
    SUB,
    AND,
    OR,
    MUL,
    DIV,
}

#[derive(Debug, Clone, Copy)]
pub enum JxxFn {
    JMP,
    JLE,
    JL,
    JE,
    JNE,
}

#[derive(Debug, Clone, Copy)]
pub enum CmovFn {}

#[derive(Debug, Clone, Copy)]
pub enum CodeFn {
    HALT,
    NOP,
    RRMOVQ,
    IRMOVQ,
    RMMOVQ,
    MRMOVQ,
    OPQ(OpqFn),
    JXX(JxxFn),
    CMOVXX(JxxFn),
    CALL,
    RET,
    PUSHQ,
    POPQ,
}

pub fn decode_register(x: u8) -> Option<Y8R> {
    match x {
        0x0 => Some(Y8R::RAX),
        0x1 => Some(Y8R::RCX),
        0x2 => Some(Y8R::RDX),
        0x3 => Some(Y8R::RBX),
        0x4 => Some(Y8R::RSP),
        0x5 => Some(Y8R::RBP),
        0x6 => Some(Y8R::RSI),
        0x7 => Some(Y8R::RDI),
        0x8 => Some(Y8R::R8),
        0x9 => Some(Y8R::R9),
        0xA => Some(Y8R::R10),
        0xB => Some(Y8R::R11),
        _ => None,
    }
}

pub fn decode_codefn(x: u8) -> Option<CodeFn> {
    match x {
        0x00 => Some(CodeFn::HALT),
        0x10 => Some(CodeFn::NOP),
        0x20 => Some(CodeFn::RRMOVQ),
        0x22 => Some(CodeFn::CMOVXX(JxxFn::JL)),
        0x23 => Some(CodeFn::CMOVXX(JxxFn::JE)),
        0x24 => Some(CodeFn::CMOVXX(JxxFn::JNE)),
        0x30 => Some(CodeFn::IRMOVQ),
        0x40 => Some(CodeFn::RMMOVQ),
        0x50 => Some(CodeFn::MRMOVQ),

        0x60 => Some(CodeFn::OPQ(OpqFn::ADD)),
        0x61 => Some(CodeFn::OPQ(OpqFn::SUB)),
        0x62 => Some(CodeFn::OPQ(OpqFn::AND)),
        0x63 => Some(CodeFn::OPQ(OpqFn::OR)),
        0x64 => Some(CodeFn::OPQ(OpqFn::MUL)),
        0x65 => Some(CodeFn::OPQ(OpqFn::DIV)),

        0x70 => Some(CodeFn::JXX(JxxFn::JMP)),
        0x71 => Some(CodeFn::JXX(JxxFn::JLE)),
        0x72 => Some(CodeFn::JXX(JxxFn::JL)),
        0x73 => Some(CodeFn::JXX(JxxFn::JE)),
        0x74 => Some(CodeFn::JXX(JxxFn::JNE)),
        0x80 => Some(CodeFn::CALL),
        0x90 => Some(CodeFn::RET),
        0xA0 => Some(CodeFn::PUSHQ),
        0xB0 => Some(CodeFn::POPQ),
        _ => None,
    }
}
