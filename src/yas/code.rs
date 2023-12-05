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
            Err(format!("Invalid value as Imm: {}", input))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
            Err(format!("Invalid input as Dest: {}", input))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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
