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
pub enum Value {
    Integer(i64),
    Label(String),
}

impl FromStr for Value {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let num_str = input.trim();
        if let Ok(num) = num_str.parse::<i64>() {
            Ok(Value::Integer(num))
        } else if num_str.chars().all(char::is_alphabetic) {
            Ok(Value::Label(num_str.to_string()))
        } else {
            Err(format!("Invalid value: {}", input))
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Address {
    value: Value,
    register: Register,
}

impl FromStr for Address {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = input.split(|c| c == '(' || c == ')').collect();
        if parts.len() == 3 {
            match parts[0].parse::<i64>() {
                Ok(offset) => {
                    let register = parts[1]
                        .parse::<Register>()
                        .map_err(|_| format!("Invalid register: {}", parts[1]))?;
                    Ok(Address {
                        value: Value::Integer(offset),
                        register,
                    })
                }
                Err(_) => {
                    if parts[0].chars().all(char::is_alphabetic) {
                        let register = parts[1]
                            .parse::<Register>()
                            .map_err(|_| format!("Invalid register: {}", parts[1]))?;
                        Ok(Address {
                            value: Value::Label(parts[0].to_string()),
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
pub enum Statement {
    Label(String),
    Halt,
    Nop,
    Rrmovq(Register, Register),
    Irmovq(Value, Register),
    Rmmovq(Register, Address),
    Mrmovq(Address, Register),
    Addq(Register, Register),
    Subq(Register, Register),
    Andq(Register, Register),
    Orq(Register, Register),
    Je(Value),
    Jne(Value),
    Call(Value),
    Ret,
    Pushq(Register),
    Popq(Register),
}

pub fn parse_statement(input: &str) -> Result<Statement, String> {
    match input.find(":") {
        Some(i) => Ok(Statement::Label(input[..i].to_string())),
        None => parse_statement_2(input),
    }
}

fn parse_statement_2(input: &str) -> Result<Statement, String> {
    let parts: Vec<&str> = input
        .trim() // 前後の空白を削除
        .split_whitespace() // 空白で分割
        .collect();
    match parts.as_slice() {
        ["halt"] => Ok(Statement::Halt),
        ["nop"] => Ok(Statement::Nop),
        ["rrmovq", ra, rb] => Ok(Statement::Rrmovq(ra.parse()?, rb.parse()?)),
        ["irmovq", v, rb] => Ok(Statement::Irmovq(v.parse()?, rb.parse()?)),
        ["rmmovq", ra, m] => Ok(Statement::Rmmovq(ra.parse()?, m.parse()?)),
        ["mrmovq", m, ra] => Ok(Statement::Mrmovq(m.parse()?, ra.parse()?)),
        ["addq", ra, rb] => Ok(Statement::Addq(ra.parse()?, rb.parse()?)),
        ["subq", ra, rb] => Ok(Statement::Subq(ra.parse()?, rb.parse()?)),
        ["andq", ra, rb] => Ok(Statement::Andq(ra.parse()?, rb.parse()?)),
        ["orq", ra, rb] => Ok(Statement::Orq(ra.parse()?, rb.parse()?)),
        ["je", m] => Ok(Statement::Je(m.parse()?)),
        ["jne", m] => Ok(Statement::Jne(m.parse()?)),
        ["call", m] => Ok(Statement::Call(m.parse()?)),
        ["ret"] => Ok(Statement::Ret),
        ["pushq", ra] => Ok(Statement::Pushq(ra.parse()?)),
        ["popq", ra] => Ok(Statement::Popq(ra.parse()?)),
        _ => Err(format!("Invalid statement: {}", input)),
    }
}

pub fn parse_body(body: &str) -> Result<Vec<Statement>, String> {
    let mut statements = Vec::new();
    for line in body.lines() {
        if !line.is_empty() {
            statements.push(parse_statement(line)?);
        }
    }
    Ok(statements)
}

pub fn byte_length(statement: &Statement) -> u64 {
    match statement {
        Statement::Label(_) => 0,
        Statement::Halt => 1,
        Statement::Nop => 1,
        Statement::Rrmovq(_, _) => 2,
        Statement::Irmovq(_, _) => 10,
        Statement::Rmmovq(_, _) => 10,
        Statement::Mrmovq(_, _) => 10,
        Statement::Addq(_, _) => 2,
        Statement::Subq(_, _) => 2,
        Statement::Andq(_, _) => 2,
        Statement::Orq(_, _) => 2,
        Statement::Je(_) => 9,
        Statement::Jne(_) => 9,
        Statement::Call(_) => 9,
        Statement::Ret => 1,
        Statement::Pushq(_) => 2,
        Statement::Popq(_) => 2,
    }
}

pub fn build_symbol_table(statements: &Vec<Statement>) -> HashMap<String, u64> {
    let mut table = HashMap::new();
    let mut addr = 0;
    for statement in statements {
        if let Statement::Label(s) = statement {
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

fn ass_val(v: &Value, symbol_table: &HashMap<String, u64>) -> Result<[u8; 8], String> {
    let x: u64 = match v {
        Value::Integer(_) => 0,
        Value::Label(s) => symbol_table
            .get(s)
            .ok_or(format!("failed to find symbol: {}", s))?
            .clone(),
    };
    let mut xs = x.to_be_bytes();
    xs.reverse();
    Result::Ok(xs)
}

fn assemble_one(
    statement: &Statement,
    symbol_table: &HashMap<String, u64>,
) -> Result<Vec<u8>, String> {
    fn f_v(head: u8, v: &Value, symbol_table: &HashMap<String, u64>) -> Result<Vec<u8>, String> {
        let xs = ass_val(v, symbol_table)?;
        Result::Ok(vec![
            head, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
        ])
    }
    match statement {
        Statement::Label(_) => Result::Ok(Vec::new()),
        Statement::Halt => Result::Ok(vec![0x00]),
        Statement::Nop => Result::Ok(vec![0x10]),
        Statement::Rrmovq(ra, rb) => Result::Ok(vec![0x20, ass_reg(Some(ra), Some(rb))]),
        Statement::Irmovq(v, rb) => {
            let r = ass_reg(None, Some(rb));
            let xs = ass_val(v, symbol_table)?;
            Result::Ok(vec![
                0x30, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }
        Statement::Rmmovq(ra, a) => {
            let r = ass_reg(Some(ra), Some(&a.register));
            let xs = ass_val(&a.value, symbol_table)?;
            Result::Ok(vec![
                0x40, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }
        Statement::Mrmovq(a, ra) => {
            let r = ass_reg(Some(ra), Some(&a.register));
            let xs = ass_val(&a.value, symbol_table)?;
            Result::Ok(vec![
                0x50, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }
        Statement::Addq(ra, rb) => Result::Ok(vec![0x60, ass_reg(Some(ra), Some(rb))]),
        Statement::Subq(ra, rb) => Result::Ok(vec![0x60, ass_reg(Some(ra), Some(rb))]),
        Statement::Andq(ra, rb) => Result::Ok(vec![0x61, ass_reg(Some(ra), Some(rb))]),
        Statement::Orq(ra, rb) => Result::Ok(vec![0x62, ass_reg(Some(ra), Some(rb))]),
        Statement::Je(v) => f_v(0x73, v, symbol_table),
        Statement::Jne(v) => f_v(0x74, v, symbol_table),
        Statement::Call(v) => f_v(0x80, v, symbol_table),
        Statement::Ret => Result::Ok(vec![0x90]),
        Statement::Pushq(ra) => Result::Ok(vec![0xA0, ass_reg(Some(ra), None)]),
        Statement::Popq(ra) => Result::Ok(vec![0xB0, ass_reg(Some(ra), None)]),
    }
}

fn assemble_many(
    statements: &Vec<Statement>,
    symbol_table: &HashMap<String, u64>,
) -> Result<Vec<u8>, String> {
    let mut xs = Vec::new();
    for s in statements {
        let mut ys = assemble_one(s, symbol_table)?;
        xs.append(&mut ys);
    }
    Result::Ok(xs)
}

pub fn assemble(body: &str) -> Result<Vec<u8>, String> {
    let statements = parse_body(body)?;
    let symbol_table = build_symbol_table(&statements);
    assemble_many(&statements, &symbol_table)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assemble() {
        let input = "halt";
        let expe = Result::Ok(vec![0x00]);
        let calc = assemble(input);
        let calc = calc;
        assert_eq!(expe, calc);
    }

    #[test]
    fn test_parse_body() {
        assert_eq!(
            parse_body("halt\nnop\nrrmovq %rax %rcx\nirmovq 100 %rax\n"),
            Ok(vec![
                Statement::Halt,
                Statement::Nop,
                Statement::Rrmovq(Register::RAX, Register::RCX),
                Statement::Irmovq(Value::Integer(100), Register::RAX),
            ]),
        );
    }

    #[test]
    fn test_symbol_table() {
        let statements: Vec<Statement> = vec![
            Statement::Halt,
            Statement::Nop,
            Statement::Label("orange".to_string()),
            Statement::Rrmovq(Register::RAX, Register::RCX),
            Statement::Label("apple".to_string()),
            Statement::Irmovq(Value::Integer(100), Register::RAX),
            Statement::Label("peach".to_string()),
        ];
        let tab = build_symbol_table(&statements);
        assert_eq!(tab.get("halt"), None);
        assert_eq!(tab.get("orange"), Some(2).as_ref());
        assert_eq!(tab.get("apple"), Some(4).as_ref());
        assert_eq!(tab.get("peach"), Some(14).as_ref());
    }

    #[test]
    fn test_parse_halt() {
        assert_eq!(parse_statement("halt"), Ok(Statement::Halt));
    }

    #[test]
    fn test_parse_nop() {
        assert_eq!(parse_statement("nop"), Ok(Statement::Nop));
    }

    #[test]
    fn test_parse_rrmovq() {
        assert_eq!(
            parse_statement("  rrmovq %rax  %rcx"),
            Ok(Statement::Rrmovq(Register::RAX, Register::RCX))
        );
    }

    #[test]
    fn test_parse_irmovq() {
        assert_eq!(
            parse_statement("irmovq 100 %rax"),
            Ok(Statement::Irmovq(Value::Integer(100), Register::RAX))
        );
    }

    #[test]
    fn test_parse_mrmovq() {
        assert_eq!(
            parse_statement("mrmovq  10(%rbx) %rax "),
            Ok(Statement::Mrmovq(
                Address {
                    value: Value::Integer(10),
                    register: Register::RBX
                },
                Register::RAX,
            ))
        );
    }

    #[test]
    fn test_parse_rmmovq() {
        assert_eq!(
            parse_statement("rmmovq    %rax  12(%rbx)  "),
            Ok(Statement::Rmmovq(
                Register::RAX,
                Address {
                    value: Value::Integer(12),
                    register: Register::RBX
                },
            ))
        );
    }

    #[test]
    fn test_parse_addq() {
        assert_eq!(
            parse_statement("addq    %rsi  %rdi  "),
            Ok(Statement::Addq(Register::RSI, Register::RDI)),
        );
    }

    #[test]
    fn test_parse_subq() {
        assert_eq!(
            parse_statement("subq    %rbp  %r8  "),
            Ok(Statement::Subq(Register::RBP, Register::R8)),
        );
    }

    #[test]
    fn test_parse_andq() {
        assert_eq!(
            parse_statement("andq    %r9  %r10  "),
            Ok(Statement::Andq(Register::R9, Register::R10)),
        );
    }

    #[test]
    fn test_parse_orq() {
        assert_eq!(
            parse_statement("orq    %rax  %r11 "),
            Ok(Statement::Orq(Register::RAX, Register::R11)),
        );
    }

    #[test]
    fn test_parse_je() {
        assert_eq!(
            parse_statement("je    100  "),
            Ok(Statement::Je(Value::Integer(100))),
        );
        assert_eq!(
            parse_statement("je    done  "),
            Ok(Statement::Je(Value::Label(String::from("done")))),
        );
    }

    #[test]
    fn test_parse_jne() {
        assert_eq!(
            parse_statement("jne    100  "),
            Ok(Statement::Jne(Value::Integer(100))),
        );
    }

    #[test]
    fn test_parse_call() {
        assert_eq!(
            parse_statement("call    100  "),
            Ok(Statement::Call(Value::Integer(100))),
        );
    }

    #[test]
    fn test_parse_ret() {
        assert_eq!(parse_statement("ret"), Ok(Statement::Ret),);
    }

    #[test]
    fn test_parse_pushq() {
        assert_eq!(
            parse_statement("pushq    %rax  "),
            Ok(Statement::Pushq(Register::RAX)),
        );
    }

    #[test]
    fn test_parse_popq() {
        assert_eq!(
            parse_statement("popq    %rax  "),
            Ok(Statement::Popq(Register::RAX)),
        );
    }
}
