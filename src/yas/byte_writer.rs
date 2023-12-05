use std::collections::HashMap;

use super::code::{Code, Dest, Imm, Register};

pub struct ByteWriter {
    codes: Vec<Code>,
    symbol_table: HashMap<String, u64>,
    pos_code: usize,
    pos_byte: usize,
}

pub struct ByteWriteError {
    pub message: String,
    pub pos: usize,
}

impl ByteWriter {
    pub fn write(codes: Vec<Code>, bytes: &mut Vec<u8>) -> Result<(), ByteWriteError> {
        let mut writer = ByteWriter::new(codes);
        writer.write_all(bytes)
    }

    pub fn new(codes: Vec<Code>) -> ByteWriter {
        let symbol_table = Self::build_symbol_table(&codes);
        ByteWriter {
            codes,
            symbol_table,
            pos_code: 0,
            pos_byte: 0,
        }
    }

    fn error<T>(&self, message: String) -> Result<T, ByteWriteError> {
        Result::Err(ByteWriteError {
            message,
            pos: self.pos_code,
        })
    }

    fn byte_write_error(&self, message: String) -> ByteWriteError {
        ByteWriteError {
            message,
            pos: self.pos_code,
        }
    }

    pub fn write_all(&mut self, bytes: &mut Vec<u8>) -> Result<(), ByteWriteError> {
        while (self.pos_code < self.codes.len()) && (self.pos_byte < bytes.len()) {
            self.write_code(bytes)?;
        }
        Result::Ok(())
    }

    fn build_symbol_table(codes: &Vec<Code>) -> HashMap<String, u64> {
        let mut table = HashMap::new();
        let mut addr = 0;
        for code in codes {
            if let Code::Label(s) = code {
                table.insert(s.to_string(), addr);
            }
            addr += Self::byte_length(&code);
        }
        table
    }

    fn get_code(&self) -> Code {
        (*self.codes.get(self.pos_code).unwrap()).clone()
    }

    fn write_code(&mut self, bytes: &mut Vec<u8>) -> Result<(), ByteWriteError> {
        let res = match self.get_code() {
            Code::Label(_) => Ok(()),
            Code::Halt => self.write_byte(0x00, bytes),
            Code::Nop => self.write_byte(0x10, bytes),

            Code::Rrmovq(ra, rb) => self.write_fn_ra_rb(0x20, &ra, &rb, bytes),
            Code::Cmovl(ra, rb) => self.write_fn_ra_rb(0x22, &ra, &rb, bytes),
            Code::Cmove(ra, rb) => self.write_fn_ra_rb(0x23, &ra, &rb, bytes),
            Code::Cmovne(ra, rb) => self.write_fn_ra_rb(0x24, &ra, &rb, bytes),

            Code::Irmovq(v, rb) => {
                self.write_byte(0x30, bytes)?;
                self.write_registers(&Register::RNONE, &rb, bytes)?;
                self.write_imm(&v, bytes)
            }
            Code::Rmmovq(ra, m) => {
                self.write_byte(0x40, bytes)?;
                self.write_registers(&ra, &m.register, bytes)?;
                self.write_dest(&m.dest, bytes)
            }
            Code::Mrmovq(a, ra) => {
                self.write_byte(0x50, bytes)?;
                self.write_registers(&ra, &a.register, bytes)?;
                self.write_dest(&a.dest, bytes)
            }

            Code::Addq(ra, rb) => self.write_fn_ra_rb(0x60, &ra, &rb, bytes),
            Code::Subq(ra, rb) => self.write_fn_ra_rb(0x61, &ra, &rb, bytes),
            Code::Andq(ra, rb) => self.write_fn_ra_rb(0x62, &ra, &rb, bytes),
            Code::Orq(ra, rb) => self.write_fn_ra_rb(0x63, &ra, &rb, bytes),
            Code::Mulq(ra, rb) => self.write_fn_ra_rb(0x64, &ra, &rb, bytes),
            Code::Divq(ra, rb) => self.write_fn_ra_rb(0x65, &ra, &rb, bytes),

            Code::Jmp(d) => self.write_fn_d(0x70, &d, bytes),
            //     // Statement::Jle(d) => f_v(0x71, d, symbol_table),
            //     // Statement::Jl(d) => f_v(0x72, d, symbol_table),
            Code::Je(d) => self.write_fn_d(0x73, &d, bytes),
            Code::Jne(d) => self.write_fn_d(0x74, &d, bytes),

            Code::Call(d) => self.write_fn_d(0x80, &d, bytes),
            Code::Ret => self.write_byte(0x90, bytes),
            Code::Pushq(ra) => self.write_fn_ra_rb(0xA0, &ra, &Register::RNONE, bytes),
            Code::Popq(ra) => self.write_fn_ra_rb(0xB0, &ra, &Register::RNONE, bytes),
        };
        self.pos_code += 1;
        res
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

    fn write_byte(&mut self, x: u8, bytes: &mut Vec<u8>) -> Result<(), ByteWriteError> {
        if self.pos_byte >= bytes.len() {
            return self.error(format!("byte length is too short: {}", self.pos_byte));
        }
        bytes[self.pos_byte] = x;
        self.pos_byte += 1;
        Result::Ok(())
    }

    fn write_quad(&mut self, x: u64, bytes: &mut Vec<u8>) -> Result<(), ByteWriteError> {
        let xs = x.to_be_bytes();
        for i in 0..8 {
            bytes[self.pos_byte + i] = xs[7 - i];
        }
        self.pos_byte += 8;
        Ok(())
    }

    fn write_fn_ra_rb(
        &mut self,
        x: u8,
        ra: &Register,
        rb: &Register,
        bytes: &mut Vec<u8>,
    ) -> Result<(), ByteWriteError> {
        self.write_byte(x, bytes)?;
        self.write_registers(ra, rb, bytes)
    }

    fn write_fn_d(&mut self, x: u8, d: &Dest, bytes: &mut Vec<u8>) -> Result<(), ByteWriteError> {
        self.write_byte(x, bytes)?;
        self.write_dest(d, bytes)
    }

    fn write_registers(
        &mut self,
        ra: &Register,
        rb: &Register,
        bytes: &mut Vec<u8>,
    ) -> Result<(), ByteWriteError> {
        fn f(r: &Register) -> u8 {
            match r {
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
                Register::RNONE => 0x0F,
            }
        }
        let x = (f(ra) << 4) + f(rb);
        self.write_byte(x, bytes)
    }

    fn write_imm(&mut self, v: &Imm, bytes: &mut Vec<u8>) -> Result<(), ByteWriteError> {
        let i: u64 = match v {
            Imm::Integer(i) => *i,
            Imm::Label(s) => self.find_symbol(s)?,
        };
        self.write_quad(i, bytes)?;
        Ok(())
    }

    fn write_dest(&mut self, d: &Dest, bytes: &mut Vec<u8>) -> Result<(), ByteWriteError> {
        let x: u64 = match d {
            Dest::Integer(i) => *i,
            Dest::Label(s) => self.find_symbol(s)?,
        };
        self.write_quad(x, bytes)?;
        Ok(())
    }

    fn find_symbol(&self, s: &str) -> Result<u64, ByteWriteError> {
        self.symbol_table
            .get(s)
            .ok_or(self.byte_write_error(format!("failed to find symbol: {}", s)))
            .map(|x| *x)
    }
}

#[cfg(test)]
mod tests {
    use crate::yas::code::ModDest;

    use super::*;

    #[test]
    fn test_byte_writer() {
        let mut memory: Vec<u8> = Vec::new();
        memory.resize(20, 0x00);
        let codes = vec![
            Code::Rmmovq(
                Register::RAX,
                ModDest {
                    register: Register::RSP,
                    dest: Dest::Integer(7),
                },
            ),
            Code::Subq(Register::RBX, Register::RCX),
        ];
        let _ = ByteWriter::write(codes, &mut memory);
        let expe: Vec<u8> = vec![
            0x40, 0x04, 0x07, 0, 0, 0, 0, 0, 0, 0, 0x61, 0x31, 0x00, 0, 0, 0, 0, 0, 0, 0,
        ];
        assert_eq!(expe, memory);
    }

    #[test]
    fn test_symbol_table() {
        let codes: Vec<Code> = vec![
            Code::Halt,
            Code::Nop,
            Code::Label("orange".to_string()),
            Code::Rrmovq(Register::RAX, Register::RCX),
            Code::Label("apple".to_string()),
            Code::Irmovq(Imm::Integer(100), Register::RAX),
            Code::Label("peach".to_string()),
        ];
        let tab = ByteWriter::build_symbol_table(&codes);
        assert_eq!(tab.get("halt"), None);
        assert_eq!(tab.get("orange"), Some(2).as_ref());
        assert_eq!(tab.get("apple"), Some(4).as_ref());
        assert_eq!(tab.get("peach"), Some(14).as_ref());
    }
}
