use std::{collections::HashMap, mem};

use super::code::{Code, Register, Expr, Directive};

pub struct ByteWriter {
    codes: Vec<Code>,
    bytes: Vec<u8>,
    symbol_table: HashMap<String, u64>,
    pos_code: usize,
    pos_byte: usize,
    first_run: bool,
    symbol_ref_table: HashMap<String, Vec<u64>>,
}

#[derive(Debug)]
pub struct ByteWriteError {
    pub message: String,
    pub pos: usize,
}

type Res<T> = Result<T, ByteWriteError>;

impl ByteWriter {
    pub fn write(codes: Vec<Code>, bytes: Vec<u8>) -> Res<Vec<u8>> {
        let mut writer = ByteWriter::new(codes, bytes);
        writer.write_all()?;
        writer.finish_first_run()?;
        writer.write_all()?;
        Ok(mem::take(&mut writer.bytes))
    }

    pub fn new(codes: Vec<Code>, bytes: Vec<u8>) -> ByteWriter {
        // let symbol_table = Self::build_symbol_table(&codes);
        ByteWriter {
            codes,
            bytes,
            symbol_table: HashMap::new(),
            pos_code: 0,
            pos_byte: 0,
            first_run: true,
            symbol_ref_table: HashMap::new(),
        }
    }

    fn finish_first_run(&mut self) -> Res<()> {
        if self.first_run {
            self.first_run = false;
            self.pos_byte = 0;
            self.pos_code = 0;
            Result::Ok(())
        } else {
            self.error("finish_first_run is called twice".to_string())
        }
    }

    fn error<T>(&self, message: String) -> Res<T> {
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

    pub fn write_all(&mut self) -> Res<()> {
        while (self.pos_code < self.codes.len()) && (self.pos_byte < self.bytes.len()) {
            self.write_code()?;
        }
        Result::Ok(())
    }

    fn get_code(&self) -> Code {
        (*self.codes.get(self.pos_code).unwrap()).clone()
    }

    fn write_code(&mut self) -> Res<()> {
        let res = match self.get_code() {
            Code::Directive(d) => self.write_directive(&d),
            Code::Label(s) => self.add_symbol(s),
            Code::Halt => self.write_byte(0x00),
            Code::Nop => self.write_byte(0x10),

            Code::Rrmovq(ra, rb) => self.write_fn_ra_rb(0x20, &ra, &rb),
            Code::Cmovl(ra, rb) => self.write_fn_ra_rb(0x22, &ra, &rb),
            Code::Cmove(ra, rb) => self.write_fn_ra_rb(0x23, &ra, &rb),
            Code::Cmovne(ra, rb) => self.write_fn_ra_rb(0x24, &ra, &rb),

            Code::Irmovq(rb, v) => {
                self.write_byte(0x30)?;
                self.write_registers(&Register::RNONE, &rb)?;
                self.write_expr(&v)
            }
            Code::Rmmovq(ra, rb, v) => {
                self.write_byte(0x40)?;
                self.write_registers(&ra, &rb)?;
                self.write_expr(&v)
            }
            Code::Mrmovq(ra, rb, v) => {
                self.write_byte(0x50)?;
                self.write_registers(&ra, &rb)?;
                self.write_expr(&v)
            }

            Code::Addq(ra, rb) => self.write_fn_ra_rb(0x60, &ra, &rb),
            Code::Subq(ra, rb) => self.write_fn_ra_rb(0x61, &ra, &rb),
            Code::Andq(ra, rb) => self.write_fn_ra_rb(0x62, &ra, &rb),
            Code::Orq(ra, rb) => self.write_fn_ra_rb(0x63, &ra, &rb),
            Code::Mulq(ra, rb) => self.write_fn_ra_rb(0x64, &ra, &rb),
            Code::Divq(ra, rb) => self.write_fn_ra_rb(0x65, &ra, &rb),

            Code::Jmp(d) => self.write_fn_expr(0x70, &d),
            //     // Statement::Jle(d) => f_v(0x71, d, symbol_table),
            //     // Statement::Jl(d) => f_v(0x72, d, symbol_table),
            Code::Je(d) => self.write_fn_expr(0x73, &d),
            Code::Jne(d) => self.write_fn_expr(0x74, &d),

            Code::Call(d) => self.write_fn_expr(0x80, &d),
            Code::Ret => self.write_byte(0x90),
            Code::Pushq(ra) => self.write_fn_ra_rb(0xA0, &ra, &Register::RNONE),
            Code::Popq(ra) => self.write_fn_ra_rb(0xB0, &ra, &Register::RNONE),
        };
        self.pos_code += 1;
        res
    }

    fn write_directive(&mut self, d: &Directive) -> Res<()> {
        match d {
            Directive::Pos(i) => {
                self.pos_byte = *i as usize;
                Result::Ok(())
            },
            Directive::Quad(i) => self.write_expr(i)
        }
    }

    fn write_byte(&mut self, x: u8) -> Res<()> {
        if self.pos_byte >= self.bytes.len() {
            return self.error(format!("byte length is too short: {}", self.pos_byte));
        }
        if !self.first_run {
            self.bytes[self.pos_byte] = x;
        }
        self.pos_byte += 1;
        Result::Ok(())
    }

    fn write_quad(&mut self, x: u64) -> Res<()> {
        let xs = x.to_be_bytes();
        for i in 0..8 {
            self.write_byte(xs[7-i])?;
        }
        Ok(())
    }

    fn write_fn_ra_rb(&mut self, x: u8, ra: &Register, rb: &Register) -> Res<()> {
        self.write_byte(x)?;
        self.write_registers(ra, rb)
    }

    fn write_fn_expr(&mut self, x: u8, v: &Expr) -> Res<()> {
        self.write_byte(x)?;
        self.write_expr(v)
    }

    fn write_registers(&mut self, ra: &Register, rb: &Register) -> Res<()> {
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
        self.write_byte(x)
    }

    fn write_expr(&mut self, v: &Expr) -> Res<()> {
        let i = match v {
            Expr::Value(i) => *i,
            Expr::Label(s) => if self.first_run {
                self.symbol_ref_table
                .entry(s.to_string())
                .or_insert(Vec::new())
                .push(self.pos_byte as u64);
                0
            } else {
                self.find_symbol(s)?
            }            
        };
        self.write_quad(i)?;
        Ok(())
    }

    fn add_symbol(&mut self, s: String) -> Res<()> {
        if self.first_run {
            self.symbol_table.insert(s, self.pos_byte as u64);
        }
        Result::Ok(())
    }

    fn find_symbol(&self, s: &str) -> Res<u64> {
        self.symbol_table
            .get(s)
            .ok_or(self.byte_write_error(format!("failed to find symbol: {}", s)))
            .map(|x| *x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_byte_writer() {
        let mut memory: Vec<u8> = Vec::new();
        memory.resize(20, 0x00);
        let codes = vec![
            Code::Rmmovq(
                Register::RAX,
                Register::RSP,
                Expr::Value(7),
            ),
            Code::Subq(Register::RBX, Register::RCX),
        ];
        let memory = ByteWriter::write(codes, memory).unwrap();
        let expe: Vec<u8> = vec![
            0x40, 0x04, 0x07, 0, 0, 0, 0, 0, 0, 0, 0x61, 0x31, 0x00, 0, 0, 0, 0, 0, 0, 0,
        ];
        assert_eq!(expe, memory);
    }

    #[test]
    fn test_byte_writer_2() {
        let codes: Vec<Code> = vec![
            Code::Directive(Directive::Pos(2)),
            Code::Label("orange".to_string()),
            Code::Rrmovq(Register::RAX, Register::RBX),
            Code::Label("apple".to_string()),
            Code::Jmp(Expr::Label("apple".to_string())),
        ];
        let mut memory: Vec<u8> = Vec::new();
        memory.resize(20, 0x00);
        let memory = ByteWriter::write(codes, memory).unwrap();
        let expe: Vec<u8> = vec![
            0, 0, 
            0x20, 0x03, 
            0x70, 0x04, 
            0, 0, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,
        ];
        assert_eq!(expe, memory);
    }
}
