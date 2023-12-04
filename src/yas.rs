/*  ask chat-GPT
以下の構文をパースするrustの関数を書いてください。 ただしパースする関数の返り値はResult型を使ってください。
statement::=halt | nop | rrmovq R, R | irmovq V, R | rmmovq R, D(R) | mrmovq D(R), R | addq R, R | subq R, R | andq R, R | orq R, R | je D | jn D | call D | ret | pushq R | popq R
D ::= {integer} | Label
R ::= $RAX | $RCX | $RDX | $RBX | $RSP | $RBP | $RSI | $RDI | $R8 | $R9 | $R10 | $R11
V ::= \${integer} | Label
Label ::= [a-zA-Z]+
*/
use std::{collections::HashMap, str::FromStr};

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Imm {
    Integer(u64),
    Label(String),
}

impl FromStr for Imm {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        if !input[0..1].eq("$") {
            return Result::Err("Invalid immediate value".to_string());
        }
        let input = &input[1..];
        let num_str = input.trim();
        if let Ok(num) = num_str.parse::<u64>() {
            Ok(Imm::Integer(num))
        } else if num_str.chars().all(char::is_alphabetic) {
            Ok(Imm::Label(num_str.to_string()))
        } else {
            Err(format!("Invalid value: {}", input))
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Dest {
    Integer(u64),
    Label(String),
}

impl FromStr for Dest {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let num_str = input.trim();
        if let Ok(num) = num_str.parse::<u64>() {
            Ok(Dest::Integer(num))
        } else if num_str.chars().all(char::is_alphabetic) {
            Ok(Dest::Label(num_str.to_string()))
        } else {
            Err(format!("Invalid value: {}", input))
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ModDest {
    pub(crate) dest: Dest,
    pub(crate) register: Register,
}

impl FromStr for ModDest {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = input.split(|c| c == '(' || c == ')').collect();
        if parts.len() == 3 {
            match parts[0].parse::<u64>() {
                Ok(offset) => {
                    let register = parts[1]
                        .parse::<Register>()
                        .map_err(|_| format!("Invalid register: {}", parts[1]))?;
                    Ok(ModDest {
                        dest: Dest::Integer(offset),
                        register,
                    })
                }
                Err(_) => {
                    if parts[0].chars().all(char::is_alphabetic) {
                        let register = parts[1]
                            .parse::<Register>()
                            .map_err(|_| format!("Invalid register: {}", parts[1]))?;
                        Ok(ModDest {
                            dest: Dest::Label(parts[0].to_string()),
                            register,
                        })
                    } else {
                        Err(format!("Invalid offset or label: {}", parts[0]))
                    }
                }
            }
        } else {
            Err(format!("Invalid address format: {}", input))
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Code {
    Label(String),
    Halt,
    Nop,
    Rrmovq(Register, Register),
    Irmovq(Imm, Register),
    Rmmovq(Register, ModDest),
    Mrmovq(ModDest, Register),
    Addq(Register, Register),
    Subq(Register, Register),
    Andq(Register, Register),
    Orq(Register, Register),
    Mulq(Register, Register),
    Divq(Register, Register),    
    Jmp(Dest),
    // Jle(Dest),
    // Jl(Dest),
    Je(Dest),
    Jne(Dest),
    Call(Dest),
    Cmovl(Register, Register),
    Cmove(Register, Register),
    Cmovne(Register, Register),
    Ret,
    Pushq(Register),
    Popq(Register),
}

fn parse_statement(input: &str) -> Result<Code, String> {
    let input = match input.find("//") {
        Some(i) => input[..i].trim(),
        None => input.trim(),
    };
    match input.find(":") {
        Some(i) => Ok(Code::Label(input[..i].to_string())),
        None => parse_instrument(input),
    }
}

fn split_instrument(input: &str) -> Vec<&str> {
    let input = input.trim_start();
    match input.find(" ") {
        None => vec![input],
        Some(i) => {
            let head = &input[..i];
            let tail = &input[i..];
            let args = tail.split(",");
        
            let mut parts = vec![head];
            for a in args {
                parts.push(a.trim());
            }
            parts
        }
    }
}

fn parse_instrument(input: &str) -> Result<Code, String> {
    let parts: Vec<&str> = split_instrument(input);
    match parts.as_slice() {
        ["halt"] => Ok(Code::Halt),
        ["nop"] => Ok(Code::Nop),
        ["rrmovq", ra, rb] => Ok(Code::Rrmovq(ra.parse()?, rb.parse()?)),
        ["irmovq", v, rb] => Ok(Code::Irmovq(v.parse()?, rb.parse()?)),
        ["rmmovq", ra, m] => Ok(Code::Rmmovq(ra.parse()?, m.parse()?)),
        ["mrmovq", m, ra] => Ok(Code::Mrmovq(m.parse()?, ra.parse()?)),
        ["addq", ra, rb] => Ok(Code::Addq(ra.parse()?, rb.parse()?)),
        ["subq", ra, rb] => Ok(Code::Subq(ra.parse()?, rb.parse()?)),
        ["andq", ra, rb] => Ok(Code::Andq(ra.parse()?, rb.parse()?)),
        ["orq", ra, rb] => Ok(Code::Orq(ra.parse()?, rb.parse()?)),
        ["mulq", ra, rb] => Ok(Code::Mulq(ra.parse()?, rb.parse()?)),
        ["divq", ra, rb] => Ok(Code::Divq(ra.parse()?, rb.parse()?)),
        ["je", m] => Ok(Code::Je(m.parse()?)),
        ["jne", m] => Ok(Code::Jne(m.parse()?)),
        ["cmove", ra, rb] => Ok(Code::Cmove(ra.parse()?, rb.parse()?)),
        ["cmovne", ra, rb] => Ok(Code::Cmovne(ra.parse()?, rb.parse()?)),
        ["call", m] => Ok(Code::Call(m.parse()?)),
        ["ret"] => Ok(Code::Ret),
        ["pushq", ra] => Ok(Code::Pushq(ra.parse()?)),
        ["popq", ra] => Ok(Code::Popq(ra.parse()?)),
        _ => Err(format!("statement parts mismatched: {0:?}", parts.as_slice())),
    }
}

pub fn parse_body(body: &str) -> Result<Vec<Code>, String> {
    let mut statements = Vec::new();
    for line in body.lines() {
        if !line.is_empty() {
            statements.push(parse_statement(line)?);
        }
    }
    Ok(statements)
}

fn byte_length(statement: &Code) -> u64 {
    match statement {
        Code::Label(_) => 0,
        Code::Halt => 1,
        Code::Nop => 1,

        Code::Rrmovq(_, _) => 2,
        Code::Irmovq(_, _) => 10,
        Code::Rmmovq(_, _) => 10,
        Code::Mrmovq(_, _) => 10,

        Code::Addq(_, _) => 2,
        Code::Subq(_, _) => 2,
        Code::Andq(_, _) => 2,
        Code::Orq(_, _) => 2,
        Code::Mulq(_, _) => 2,
        Code::Divq(_, _) => 2,        

        Code::Jmp(_) => 9,
        // Statement::Jle(_) => 9,
        // Statement::Jl(_) => 9,
        Code::Je(_) => 9,
        Code::Jne(_) => 9,

        Code::Cmovl(_, _) => 2,
        Code::Cmove(_, _) => 2,
        Code::Cmovne(_, _) => 2,

        Code::Call(_) => 9,
        Code::Ret => 1,
        Code::Pushq(_) => 2,
        Code::Popq(_) => 2,
    }
}

pub fn build_symbol_table(statements: &Vec<Code>) -> HashMap<String, u64> {
    let mut table = HashMap::new();
    let mut addr = 0;
    for statement in statements {
        if let Code::Label(s) = statement {
            table.insert(s.to_string(), addr);
        }
        addr += byte_length(&statement);
    }
    table
}

fn ass_reg(ra: Option<&Register>, rb: Option<&Register>) -> u8 {
    fn f(r: Option<&Register>) -> u8 {
        match r {
            None => 0x0F,
            Some(r) => match r {
                Register::RAX => 0x00,
                Register::RCX => 0x01,
                Register::RDX => 0x02,
                Register::RBX => 0x03,
                Register::RSP => 0x04,
                Register::RBP => 0x05,
                Register::RSI => 0x06,
                Register::RDI => 0x07,
                Register::R8 => 0x08,
                Register::R9 => 0x09,
                Register::R10 => 0x0A,
                Register::R11 => 0x0B,
            },
        }
    }
    (f(ra) << 4) + f(rb)
}

fn ass_imm(v: &Imm, symbol_table: &HashMap<String, u64>) -> Result<[u8; 8], String> {
    let x: u64 = match v {
        Imm::Integer(i) => *i,
        Imm::Label(s) => symbol_table
            .get(s)
            .ok_or(format!("failed to find symbol: {}", s))?
            .clone(),
    };
    let mut xs = x.to_be_bytes();
    xs.reverse();
    Result::Ok(xs)
}

fn ass_dest(d: &Dest, symbol_table: &HashMap<String, u64>) -> Result<[u8; 8], String> {
    // FIXME: duplicated code
    let x: u64 = match d {
        Dest::Integer(i) => *i,
        Dest::Label(s) => symbol_table
            .get(s)
            .ok_or(format!("failed to find symbol: {}", s))?
            .clone(),
    };
    let mut xs = x.to_be_bytes();
    xs.reverse();
    Result::Ok(xs)
}

fn assemble_one(
    statement: &Code,
    symbol_table: &HashMap<String, u64>,
) -> Result<Vec<u8>, String> {
    fn f_v(head: u8, d: &Dest, symbol_table: &HashMap<String, u64>) -> Result<Vec<u8>, String> {
        let xs = ass_dest(d, symbol_table)?;
        Result::Ok(vec![
            head, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
        ])
    }
    match statement {
        Code::Label(_) => Result::Ok(Vec::new()),
        Code::Halt => Result::Ok(vec![0x00]),
        Code::Nop => Result::Ok(vec![0x10]),

        Code::Rrmovq(ra, rb) => Result::Ok(vec![0x20, ass_reg(Some(ra), Some(rb))]),
        Code::Irmovq(v, rb) => {
            let r = ass_reg(None, Some(rb));
            let xs = ass_imm(v, symbol_table)?;
            Result::Ok(vec![
                0x30, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }
        Code::Rmmovq(ra, a) => {
            let r = ass_reg(Some(ra), Some(&a.register));
            let xs = ass_dest(&a.dest, symbol_table)?;
            Result::Ok(vec![
                0x40, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }
        Code::Mrmovq(a, ra) => {
            let r = ass_reg(Some(ra), Some(&a.register));
            let xs = ass_dest(&a.dest, symbol_table)?;
            Result::Ok(vec![
                0x50, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }

        Code::Addq(ra, rb) => Result::Ok(vec![0x60, ass_reg(Some(ra), Some(rb))]),
        Code::Subq(ra, rb) => Result::Ok(vec![0x61, ass_reg(Some(ra), Some(rb))]),
        Code::Andq(ra, rb) => Result::Ok(vec![0x62, ass_reg(Some(ra), Some(rb))]),
        Code::Orq(ra, rb) => Result::Ok(vec![0x63, ass_reg(Some(ra), Some(rb))]),
        Code::Mulq(ra, rb) => Result::Ok(vec![0x64, ass_reg(Some(ra), Some(rb))]),
        Code::Divq(ra, rb) => Result::Ok(vec![0x65, ass_reg(Some(ra), Some(rb))]),

        Code::Jmp(d) => f_v(0x70, d, symbol_table),
        // Statement::Jle(d) => f_v(0x71, d, symbol_table),        
        // Statement::Jl(d) => f_v(0x72, d, symbol_table),
        Code::Je(d) => f_v(0x73, d, symbol_table),
        Code::Jne(d) => f_v(0x74, d, symbol_table),

        Code::Cmovl(ra, rb) => Result::Ok(vec![0x22, ass_reg(Some(ra), Some(rb))]),
        Code::Cmove(ra, rb) => Result::Ok(vec![0x23, ass_reg(Some(ra), Some(rb))]),
        Code::Cmovne(ra, rb) => Result::Ok(vec![0x24, ass_reg(Some(ra), Some(rb))]),        

        Code::Call(d) => f_v(0x80, d, symbol_table),
        Code::Ret => Result::Ok(vec![0x90]),
        Code::Pushq(ra) => Result::Ok(vec![0xA0, ass_reg(Some(ra), None)]),
        Code::Popq(ra) => Result::Ok(vec![0xB0, ass_reg(Some(ra), None)]),
    }
}

pub fn assemble_many(
    statements: &Vec<Code>,
    symbol_table: &HashMap<String, u64>,
) -> Result<Vec<u8>, String> {
    let mut xs = Vec::new();
    for s in statements {
        let mut ys = assemble_one(s, symbol_table)?;
        xs.append(&mut ys);
    }
    Result::Ok(xs)
}

pub fn code(statements: &Vec<Code>) -> Result<Vec<u8>, String> {
    let symbol_table = build_symbol_table(&statements);
    assemble_many(&statements, &symbol_table)
}

// fn assemble(body: &str) -> Result<Vec<u8>, String> {
//     let statements = parse_body(body)?;
//     code(&statements)
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ass_imm() {
        let v = Imm::Integer(9);
        let st = HashMap::new();
        let calc = ass_imm(&v, &st);
        let expe: [u8; 8] = [9, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(Result::Ok(expe), calc);

        let v = Imm::Label(String::from("apple"));
        let mut st = HashMap::new();
        st.insert(String::from("apple"), 13);
        let calc = ass_imm(&v, &st);
        let expe: [u8; 8] = [13, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(Result::Ok(expe), calc);
    }

    #[test]
    fn test_assemble() {
        let input = "halt";
        let expe = Result::Ok(vec![0x00]);
        let calc = parse_body(input);
        let calc = calc.and_then(|x| code(&x));
        // let calc = assemble(input);
        // let calc = calc;
        assert_eq!(expe, calc);
    }

    #[test]
    fn test_parse_body() {
        assert_eq!(
            parse_body("halt\nnop\nrrmovq %rax, %rcx\nirmovq $100, %rax\n"),
            Ok(vec![
                Code::Halt,
                Code::Nop,
                Code::Rrmovq(Register::RAX, Register::RCX),
                Code::Irmovq(Imm::Integer(100), Register::RAX),
            ]),
        );
    }

    #[test]
    fn test_symbol_table() {
        let statements: Vec<Code> = vec![
            Code::Halt,
            Code::Nop,
            Code::Label("orange".to_string()),
            Code::Rrmovq(Register::RAX, Register::RCX),
            Code::Label("apple".to_string()),
            Code::Irmovq(Imm::Integer(100), Register::RAX),
            Code::Label("peach".to_string()),
        ];
        let tab = build_symbol_table(&statements);
        assert_eq!(tab.get("halt"), None);
        assert_eq!(tab.get("orange"), Some(2).as_ref());
        assert_eq!(tab.get("apple"), Some(4).as_ref());
        assert_eq!(tab.get("peach"), Some(14).as_ref());
    }

    #[test]
    fn test_parse_halt() {
        assert_eq!(parse_statement("halt   //comment"), Ok(Code::Halt));
    }

    #[test]
    fn test_parse_nop() {
        assert_eq!(parse_statement("nop"), Ok(Code::Nop));
    }

    #[test]
    fn test_parse_rrmovq() {
        assert_eq!(
            parse_statement("  rrmovq %rax , %rcx"),
            Ok(Code::Rrmovq(Register::RAX, Register::RCX))
        );
    }

    #[test]
    fn test_parse_irmovq() {
        assert_eq!(
            parse_statement("irmovq $100, %rax"),
            Ok(Code::Irmovq(Imm::Integer(100), Register::RAX))
        );

        assert_eq!(
            parse_statement("mrmovq  10(%rbx), %rax "),
            Ok(Code::Mrmovq(
                ModDest {
                    dest: Dest::Integer(10),
                    register: Register::RBX
                },
                Register::RAX,
            ))
        );

        assert_eq!(
            parse_statement("rmmovq    %rax , 12(%rbx)  "),
            Ok(Code::Rmmovq(
                Register::RAX,
                ModDest {
                    dest: Dest::Integer(12),
                    register: Register::RBX
                },
            ))
        );

        assert_eq!(
            parse_statement("cmove    %rax , %rbx  "),
            Ok(Code::Cmove(
                Register::RAX,
                Register::RBX))
        );

        assert_eq!(
            parse_statement("cmovne    %rax , %rbx  "),
            Ok(Code::Cmovne(
                Register::RAX,
                Register::RBX))
        );
    }

    #[test]
    fn test_parse_opq() {
        assert_eq!(
            parse_statement("  addq    %rsi,  %rdi  "),
            Ok(Code::Addq(Register::RSI, Register::RDI)),
        );

        assert_eq!(
            parse_statement("subq    %rbp , %r8  "),
            Ok(Code::Subq(Register::RBP, Register::R8)),
        );
    
        assert_eq!(
            parse_statement("andq    %r9,  %r10  "),
            Ok(Code::Andq(Register::R9, Register::R10)),
        );

        assert_eq!(
            parse_statement("orq    %rax  ,%r11 "),
            Ok(Code::Orq(Register::RAX, Register::R11)),
        );

        assert_eq!(
            parse_statement("  mulq  %r9,  %r10  "),
            Ok(Code::Mulq(Register::R9, Register::R10)),
        );

        assert_eq!(
            parse_statement(" divq  %rax  ,%r11 "),
            Ok(Code::Divq(Register::RAX, Register::R11)),
        );
    }

    #[test]
    fn test_parse_jxx() {
        assert_eq!(
            parse_statement("je    100  "),
            Ok(Code::Je(Dest::Integer(100))),
        );
        assert_eq!(
            parse_statement("je    done  "),
            Ok(Code::Je(Dest::Label(String::from("done")))),
        );

        assert_eq!(
            parse_statement("jne    100  "),
            Ok(Code::Jne(Dest::Integer(100))),
        );
    }

    #[test]
    fn test_parse_call() {
        assert_eq!(
            parse_statement("call    100  "),
            Ok(Code::Call(Dest::Integer(100))),
        );
    }

    #[test]
    fn test_parse_ret() {
        assert_eq!(parse_statement("ret"), Ok(Code::Ret),);
    }

    #[test]
    fn test_parse_pushq() {
        assert_eq!(
            parse_statement("pushq    %rax  "),
            Ok(Code::Pushq(Register::RAX)),
        );
    }

    #[test]
    fn test_parse_popq() {
        assert_eq!(
            parse_statement("popq    %rax  "),
            Ok(Code::Popq(Register::RAX)),
        );
    }
}
