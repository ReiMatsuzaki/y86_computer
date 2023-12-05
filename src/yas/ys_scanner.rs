use super::code::{Code, Dest, Imm, ModDest, Register};

#[derive(Debug, PartialEq)]
pub struct ScanError {
    line_num: usize,
    pub message: String,
}

pub struct Scanner {
    line_num: usize,
}

impl Scanner {
    pub fn new() -> Scanner {
        Scanner { line_num: 0 }
    }

    pub fn scan(input: &str) -> Result<Vec<Code>, ScanError> {
        Scanner::new().scan_src(input)
    }

    fn error<T>(&self, message: String) -> Result<T, ScanError> {
        Err(ScanError {
            line_num: self.line_num,
            message,
        })
    }

    pub fn scan_src(&mut self, src: &str) -> Result<Vec<Code>, ScanError> {
        let mut statements = Vec::new();
        for line in src.lines() {
            let line = line.trim();
            if !line.is_empty() {
                statements.push(self.scan_line(line)?);
            }
            self.line_num += 1;
        }
        Ok(statements)
    }

    fn scan_line(&self, input: &str) -> Result<Code, ScanError> {
        let input = match input.find("//") {
            Some(i) => input[..i].trim(),
            None => input.trim(),
        };
        match input.find(":") {
            Some(i) => Ok(Code::Label(input[..i].to_string())),
            None => self.scan_command(input),
        }
    }

    fn scan_command(&self, input: &str) -> Result<Code, ScanError> {
        let parts: Vec<&str> = Self::split_command(input);
        match parts.as_slice() {
            ["halt"] => Ok(Code::Halt),
            ["nop"] => Ok(Code::Nop),
            ["rrmovq", ra, rb] => Ok(Code::Rrmovq(
                self.scan_register(ra)?,
                self.scan_register(rb)?,
            )),
            ["irmovq", v, rb] => Ok(Code::Irmovq(self.scan_value(v)?, self.scan_register(rb)?)),
            ["rmmovq", ra, m] => Ok(Code::Rmmovq(
                self.scan_register(ra)?,
                self.scan_mod_dest(m)?,
            )),
            ["mrmovq", m, ra] => Ok(Code::Mrmovq(
                self.scan_mod_dest(m)?,
                self.scan_register(ra)?,
            )),
            ["addq", ra, rb] => Ok(Code::Addq(self.scan_register(ra)?, self.scan_register(rb)?)),
            ["subq", ra, rb] => Ok(Code::Subq(self.scan_register(ra)?, self.scan_register(rb)?)),
            ["andq", ra, rb] => Ok(Code::Andq(self.scan_register(ra)?, self.scan_register(rb)?)),
            ["orq", ra, rb] => Ok(Code::Orq(self.scan_register(ra)?, self.scan_register(rb)?)),
            ["mulq", ra, rb] => Ok(Code::Mulq(self.scan_register(ra)?, self.scan_register(rb)?)),
            ["divq", ra, rb] => Ok(Code::Divq(self.scan_register(ra)?, self.scan_register(rb)?)),
            ["je", m] => Ok(Code::Je(self.scan_dest(m)?)),
            ["jne", m] => Ok(Code::Jne(self.scan_dest(m)?)),
            ["cmove", ra, rb] => Ok(Code::Cmove(
                self.scan_register(ra)?,
                self.scan_register(rb)?,
            )),
            ["cmovne", ra, rb] => Ok(Code::Cmovne(
                self.scan_register(ra)?,
                self.scan_register(rb)?,
            )),
            ["call", m] => Ok(Code::Call(self.scan_dest(m)?)),
            ["ret"] => Ok(Code::Ret),
            ["pushq", ra] => Ok(Code::Pushq(self.scan_register(ra)?)),
            ["popq", ra] => Ok(Code::Popq(self.scan_register(ra)?)),
            _ => self
                .error(format!("statement parts mismatched: {0:?}", parts.as_slice()).to_string()),
        }
    }

    fn scan_value(&self, input: &str) -> Result<Imm, ScanError> {
        println!("scan_value");
        match input.parse() {
            Ok(i) => Ok(i),
            Err(_) => self.error(format!("invalid Imm: {}", input)),
        }
    }

    fn scan_dest(&self, input: &str) -> Result<Dest, ScanError> {
        match input.parse() {
            Ok(i) => Ok(Dest::Integer(i)),
            Err(_) => Ok(Dest::Label(input.to_string())),
        }
    }

    fn scan_mod_dest(&self, input: &str) -> Result<ModDest, ScanError> {
        match input.parse() {
            Ok(i) => Ok(i),
            Err(e) => self.error(format!("invalid mod dest: {}", e)),
        }
    }

    fn scan_register(&self, input: &str) -> Result<Register, ScanError> {
        match input.parse() {
            Ok(r) => Ok(r),
            Err(e) => self.error(e),
        }
    }

    fn split_command(input: &str) -> Vec<&str> {
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
}

#[cfg(test)]
mod tests {
    use crate::yas::code::{Dest, Imm, ModDest, Register};

    use super::*;

    #[test]
    fn test_scan_body() {
        let x = "
        halt
        nop
        rrmovq %rax, %rcx
        irmovq $100, %rax
        mrmovq  10(%rbx), %rax 
        rmmovq    %rax , 12(%rbx)
        cmove    %rax , %rbx
        cmovne   %rax , %rbx
        addq    %rsi,  %rdi
        subq    %rbp , %r8
        andq    %r9,  %r10
        orq    %rax  ,%r11
        mulq  %r9,  %r10  
        divq  %rax  ,%r11 
        je    100  
        je    done  
        jne    100 
         call    100  
         pushq    %rax  
         popq    %rax                  
        ";
        assert_eq!(
            Scanner::scan(x),
            Ok(vec![
                Code::Halt,
                Code::Nop,
                Code::Rrmovq(Register::RAX, Register::RCX),
                Code::Irmovq(Imm::Integer(100), Register::RAX),
                Code::Mrmovq(
                    ModDest {
                        dest: Dest::Integer(10),
                        register: Register::RBX
                    },
                    Register::RAX,
                ),
                Code::Rmmovq(
                    Register::RAX,
                    ModDest {
                        dest: Dest::Integer(12),
                        register: Register::RBX
                    },
                ),
                Code::Cmove(Register::RAX, Register::RBX),
                Code::Cmovne(Register::RAX, Register::RBX),
                Code::Addq(Register::RSI, Register::RDI),
                Code::Subq(Register::RBP, Register::R8),
                Code::Andq(Register::R9, Register::R10),
                Code::Orq(Register::RAX, Register::R11),
                Code::Mulq(Register::R9, Register::R10),
                Code::Divq(Register::RAX, Register::R11),
                Code::Je(Dest::Integer(100)),
                Code::Je(Dest::Label(String::from("done"))),
                Code::Jne(Dest::Integer(100)),
                Code::Call(Dest::Integer(100)),
                Code::Pushq(Register::RAX),
                Code::Popq(Register::RAX),
            ]),
        );
    }
}
