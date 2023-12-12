use std::fmt;

use super::inst::*;
use super::ram::*;

// const MEM_SIZE: usize = 0x10000;
// const EXCEPTION_TABLE_BASE: usize = 0xE200;

#[derive(Debug, Clone)]
struct Fetched {
    code_fn: CodeFn,
    ra: u8,
    rb: u8,
    val_c: u64,
    val_p: usize,
}

impl fmt::Display for Fetched {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn g(r: u8) -> String {
            match decode_register(r) {
                Some(r) => format!("{:?}", r),
                None => "None".to_string(),
            }
        }
        let ra = g(self.ra);
        let rb = g(self.rb);
        write!(
            f,
            "code_fn= {:?}, ra: {1:?}, rb: {2:?}, c: 0x{3:X}",
            self.code_fn, ra, rb, self.val_c
        )
    }
}

#[derive(Debug, Clone)]
struct Decoded {
    val_a: u64,
    val_b: u64,
    dst_e: usize,
    dst_m: usize,
}

#[derive(Debug, Clone)]
struct Executed {
    val_e: u64,
}

#[derive(Debug)]
pub struct Memoried {
    val_m: u64,
}

pub struct Cpu {
    regs: [u64; 16],
    pc: usize,
    zf: u8,
    sf: u8,
    of: u8,
    stat: Y8S,

    verbose: i64,
}


fn split_byte(x: u8) -> (u8, u8) {
    return (x >> 4, x & 0x0F);
}

type Res<T> = Result<T, Exception>;

impl Cpu {
    pub fn new(verbose: i64, pc: usize) -> Cpu {
        let machine = Cpu {
            regs: [0; 16],
            pc,
            zf: 0,
            sf: 0,
            of: 0,
            stat: Y8S::AOK,
            verbose,    
        };
        return machine;
    }
    fn fetch(&mut self, ram: &mut Ram) -> Res<Fetched> {
        self.stat = Y8S::AOK;

        let code_fn = match decode_codefn(ram.read(self.pc)) {
            Some(CodeFn::HALT) => {
                self.stat = Y8S::HLT;
                CodeFn::HALT
            }
            Some(code_fn) => code_fn,
            None => {
                self.stat = Y8S::INS;
                CodeFn::NOP
            }
        };

        let (ra, rb) = split_byte(ram.read(self.pc + 1));
        let (val_p, c0): (usize, usize) = match code_fn {
            CodeFn::HALT => (self.pc + 1, self.pc + 2),
            CodeFn::NOP => (self.pc + 1, self.pc + 2),
            CodeFn::RRMOVQ => (self.pc + 2, self.pc + 2),
            CodeFn::IRMOVQ => (self.pc + 10, self.pc + 2),
            CodeFn::RMMOVQ => (self.pc + 10, self.pc + 2),
            CodeFn::MRMOVQ => (self.pc + 10, self.pc + 2),
            CodeFn::OPQ(_) => (self.pc + 2, self.pc + 2),
            CodeFn::JXX(_) => (self.pc + 9, self.pc + 1),
            CodeFn::CMOVXX(_) => (self.pc + 2, self.pc + 2),
            CodeFn::CALL => (self.pc + 9, self.pc + 1),
            CodeFn::RET => (self.pc + 1, self.pc + 1),
            CodeFn::PUSHQ => (self.pc + 2, self.pc + 2),
            CodeFn::POPQ => (self.pc + 2, self.pc + 2),
        };
        // let val_c: u64 = self.memory[c0].into();
        let val_c: u64 = ram.read_quad(c0);
        Ok(Fetched {
            code_fn,
            ra,
            rb,
            val_c,
            val_p,
        })
    }
    fn decode(&self, fetched: &Fetched) -> Res<Decoded> {
        let ra = fetched.ra as usize;
        let rb = fetched.rb as usize;
        let rsp = Y8R::RSP as usize;
        let (src_a, src_b) = match fetched.code_fn {
            CodeFn::PUSHQ => (ra, rsp),
            CodeFn::POPQ => (rsp, rsp),
            CodeFn::CALL => (0xF, rsp),
            CodeFn::RET => (rsp, rsp),
            _ => (ra, rb),
        };
        let (dst_e, dst_m) = match fetched.code_fn {
            CodeFn::HALT => (0xF, 0xF),
            CodeFn::NOP => (0xF, 0xF),
            CodeFn::IRMOVQ => (src_b, 0xF),
            CodeFn::RRMOVQ => (src_b, 0xF),
            CodeFn::RMMOVQ => (0xF, 0xF),
            CodeFn::MRMOVQ => (0xF, src_a),
            CodeFn::OPQ(_) => (src_b, 0xF),
            CodeFn::JXX(_) => (0xF, 0xF),
            CodeFn::CMOVXX(JxxFn::JMP) => (src_b, 0xF),
            CodeFn::CMOVXX(JxxFn::JLE) => (
                if self.sf == 1 || self.zf == 1 {
                    src_b
                } else {
                    0xF
                },
                0xF,
            ),
            CodeFn::CMOVXX(JxxFn::JL) => (
                if self.sf == 1 && self.zf == 0 {
                    src_b
                } else {
                    0xF
                },
                0xF,
            ),
            CodeFn::CMOVXX(JxxFn::JE) => (if self.zf == 1 { src_b } else { 0xF }, 0xF),
            CodeFn::CMOVXX(JxxFn::JNE) => (if self.zf == 1 { 0xF } else { src_b }, 0xF),
            CodeFn::CALL => (rsp, 0xF),
            CodeFn::RET => (rsp, 0xF),
            CodeFn::PUSHQ => (rsp, 0xF),
            CodeFn::POPQ => (rsp, ra),
        };
        let val_a: u64 = self.regs[src_a];
        let val_b: u64 = self.regs[src_b];
        Ok(Decoded {
            val_a,
            val_b,
            dst_e,
            dst_m,
        })
    }
    fn execute(&mut self, f: &Fetched, d: &Decoded) -> Res<Executed> {
        let va = d.val_a;
        let vb = d.val_b;
        let vc = f.val_c;
        let val_e: u64 = match f.code_fn {
            CodeFn::HALT => 0,
            CodeFn::NOP => 0,
            CodeFn::RRMOVQ => va,
            CodeFn::IRMOVQ => vc,
            CodeFn::RMMOVQ => vb + vc,
            CodeFn::MRMOVQ => vb + vc,
            CodeFn::OPQ(f) => match f {
                OpqFn::ADD => va.wrapping_add(vb),
                OpqFn::SUB => vb.wrapping_sub(va),
                OpqFn::AND => va & vb,
                OpqFn::OR => va | vb,
                OpqFn::MUL => va * vb,
                OpqFn::DIV => if va==0 { 0 } else {vb / va}
            },
            CodeFn::JXX(_) => 0,
            CodeFn::CMOVXX(_) => va,
            CodeFn::CALL => vb - 8, // R[%rsp] - 8
            CodeFn::RET => vb + 8,
            CodeFn::PUSHQ => vb - 8, // R[%rsp] - 8
            CodeFn::POPQ => vb + 8,  // R[%rsp] + 8
        };
        match f.code_fn {
            CodeFn::OPQ(_) => {
                self.zf = if val_e == 0 { 0x1 } else { 0x0 };
                self.sf = if val_e > !val_e { 0x1 } else { 0x0 };
                self.of = 0x0; // FIXME
            }
            _ => {}
        }
        Ok(Executed { val_e })
    }
    fn memory(&mut self, f: &Fetched, d: &Decoded, e: &Executed, ram: &mut Ram) -> Res<Memoried> {
        let val_m = match f.code_fn {
            CodeFn::HALT => 0,
            CodeFn::NOP => 0,
            CodeFn::RRMOVQ => 0,
            CodeFn::IRMOVQ => 0,
            CodeFn::RMMOVQ => {
                let addr = e.val_e as usize;
                if ram.size() <= addr {
                    return Err(Exception::ProtectionFault);
                }
                ram.write_quad(addr, d.val_a);
                0
            }
            CodeFn::MRMOVQ => {
                let addr = e.val_e as usize;
                ram.read_quad(addr)
            }
            CodeFn::OPQ(_) => 0,
            CodeFn::JXX(_) => 0,
            CodeFn::CMOVXX(_) => 0,
            CodeFn::CALL => {
                let addr = e.val_e as usize;
                ram.write_quad(addr, f.val_p as u64);
                0
            }
            CodeFn::PUSHQ => {
                let addr = e.val_e as usize;
                ram.write_quad(addr, d.val_a);
                0
            }
            CodeFn::RET | CodeFn::POPQ => {
                let addr = d.val_a as usize;
                ram.read_quad(addr)
            }
        };
        Ok(Memoried { val_m })
    }
    fn write(&mut self, f: &Fetched, d: &Decoded, e: &Executed, m: &Memoried) {
        self.regs[d.dst_e as usize] = e.val_e;
        self.regs[d.dst_m as usize] = m.val_m;
        let val_c = f.val_c as usize;
        let val_p = f.val_p;
        let val_m = m.val_m as usize;

        self.pc = match f.code_fn {
            CodeFn::CALL => val_c,
            CodeFn::RET => val_m,
            CodeFn::JXX(j) => {
                let jump = match j {
                    JxxFn::JMP => true,
                    JxxFn::JLE => self.sf == 1 || self.zf == 1,
                    JxxFn::JL => self.sf == 1 && self.zf == 0,
                    JxxFn::JE => self.zf == 1,
                    JxxFn::JNE => self.zf == 0,
                };
                if jump {
                    val_c
                } else {
                    val_p
                }
            }
            _ => f.val_p,
        };
    }
    pub fn print_registers(&self) {
        print!("pc=0x{0:0>2X}, ", self.pc);
        print!("RAX=0x{0:X}, RBX=0x{1:X}, RCX=0x{2:X}, RDX=0x{3:X}, RSP=0x{4:X}, RBP=0x{5:X}, RSI=0x{6:X}, RDI=0x{7:X}, ",
          self.get_register(Y8R::RAX),
          self.get_register(Y8R::RBX),
          self.get_register(Y8R::RCX),
          self.get_register(Y8R::RDX),
          self.get_register(Y8R::RSP),
          self.get_register(Y8R::RBP),
          self.get_register(Y8R::RSI),
          self.get_register(Y8R::RDI),
      );
        println!("ZF={0}, SF={1}", self.zf, self.sf);
        // println!(" R8=0x{0:X},  R9=0x{1:X}, R10=0x{2:X}, R11=0x{3:X}",
        //     self.regs[Y8R::R8 as usize],
        //     self.regs[Y8R::R9 as usize],
        //     self.regs[Y8R::R10 as usize],
        //     self.regs[Y8R::R11 as usize],
        // );
    }
    pub fn cycle(&mut self, ram: &mut Ram) -> Res<Y8S> {
        let fetched = self.fetch(ram)?;
        if self.verbose >= 2 {
            println!("fetched: {}", fetched);
        }
        let decoded = self.decode(&fetched)?;
        if self.verbose >= 3 {
            println!("{:?}", decoded);
        }
        let executed = self.execute(&fetched, &decoded)?;
        if self.verbose >= 3 {
            println!("{:?}", executed);
        }
        let memoried = self.memory(&fetched, &decoded, &executed, ram)?;
        if self.verbose >= 3 {
            println!("{:?}", memoried);
        }
        self.write(&fetched, &decoded, &executed, &memoried);

        if fetched.code_fn == CodeFn::OPQ(OpqFn::DIV) && decoded.val_a == 0 {
            return Err(Exception::DivideError);
        }

        Ok(self.stat.clone())
    }
    pub fn get_register(&self, r: Y8R) -> u64 {
        return self.regs[r as usize];
    }
    pub fn set_register(&mut self, r: Y8R, value: u64) {
        self.regs[r as usize] = value;
    }
    pub fn set_pc(&mut self, pc: usize) {
        self.pc = pc;
    }
    pub fn get_pc(&self) -> usize {
        self.pc
    }
}

#[cfg(test)]
impl Cpu {
    pub fn start(&mut self, ram: &mut Ram) -> Option<u64> {
        for cyc in 0..1000 {
            self.cycle(ram).unwrap();
            // self.print_registers();
            // ram.print(Some(0x00), Some(0x90));
            if self.stat == Y8S::HLT {
                return Some(cyc);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const MEM_SIZE: usize = 0x10000;

    #[test]
    fn split_byte_test() {
        let x: u8 = 0xAB;
        let (y, z) = split_byte(x);
        assert_eq!(0x0A, y);
        assert_eq!(0x0B, z);
    }

    // #[test]
    // fn seq_processor_test() {
    //     let mut ram = Ram::new(MEM_SIZE);
    //     let mut machine = SeqProcessor::new(2, 0);
    //     let insts: [u8; 4 * 10 + 2 * 9 + 3 * 2 + 2 * 1] = [
    //         // 0x00: IRMOVQ $9  $rdx
    //         0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0, 
    //         // 0x0A: IRMOVQ $21 $rbx
    //         0x30, 0xF3, 0x15, 0, 0, 0, 0, 0, 0, 0, 
    //         // 0x14: subq rdx rdx
    //         0x61, 0x22,
    //         // 0x16: IRMOVQ $128 $rsp
    //         0x30, 0xF4, 0x80, 0, 0, 0, 0, 0, 0, 0,
    //         // -> rsp=0x80
    //         // 0x20: RMMOVQ $rsp 100(%rbx)
    //         0x40, 0x43, 0x64, 0, 0, 0, 0, 0, 0, 0,
    //         // -> M[0x79]=0x80    // 0x64+0x15=0x79
    //         // 0x02A: PUSHQ $rdx
    //         0xA0, 0x2F,
    //         // -> M[0x78]=0x09    // 0x80-0x08=0x78
    //         // 0x2C: POPQ $rax
    //         0xB0, 0x0F, // -> rax=0x09
    //         // 0x2E: je done
    //         0x73, 0x40, 0, 0, 0, 0, 0, 0, 0, 
    //         // 0x37: call proc
    //         0x80, 0x41, 0, 0, 0, 0, 0, 0, 0, 
    //         // 0x40: halt
    //         0x00, 
    //         // 0x41: ret
    //         0x90,
    //     ];
    //     ram.load(0, &insts);
    //     machine.start(&mut ram);
    //     assert_eq!(0x80, ram.read(0x79));
    //     assert_eq!(0x40, ram.read(0x71));
    //     assert_eq!(0x09, machine.get_register(Y8R::RAX));
    // }

    #[test]
    fn opq_test() {
        let mut ram = Ram::new(MEM_SIZE);
        let insts: [u8; 2 * 10 + 2] = [
            // IRMOVQ $9  $rdx
            0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $4 $rbx
            0x30, 0xF3, 0x04, 0, 0, 0, 0, 0, 0, 0, // subq rdx rbx
            0x61, 0x23,
            // -> rdx=0x09, rbx=-5
        ];
        let mut machine = Cpu::new(0, 0);
        ram.load(0, &insts);
        machine.start(&mut ram);
        let neg: u64 = 5;
        assert_eq!(0, neg.wrapping_add(machine.get_register(Y8R::RBX)));

        let insts: [u8; 2 * 10 + 2 * 2] = [
            // IRMOVQ $9 $rdx
            0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $4 $rbx
            0x30, 0xF3, 0x04, 0, 0, 0, 0, 0, 0, 0, // subq rdx rbx
            0x61, 0x23, // -> rdx=0x09, rbx=4-9=-5
            // addq rdx rbx
            0x60, 0x23, // -> rbx=4
        ];
        let mut machine = Cpu::new(0, 0);
        ram.load(0, &insts);
        machine.start(&mut ram);
        assert_eq!(0x04, machine.get_register(Y8R::RBX));

        let insts: [u8; 4 * 10 + 2 * 2] = [
            // IRMOVQ $9  $rdx
            0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $4 $rbx
            0x30, 0xF3, 0x04, 0, 0, 0, 0, 0, 0, 0, // mulq rdx rbx
            0x64, 0x23, // -> rdx=0x09, rbx=36=0x24

            // IRMOVQ $4  $rdx
            0x30, 0xF2, 0x04, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $9 $rax
            0x30, 0xF0, 0x09, 0, 0, 0, 0, 0, 0, 0, // divq rdx rax
            0x65, 0x20,
            // -> rax=0x02
        ];
        let mut machine = Cpu::new(0, 0);
        ram.load(0, &insts);
        machine.start(&mut ram);
        assert_eq!(0x24, machine.get_register(Y8R::RBX));
        assert_eq!(0x02, machine.get_register(Y8R::RAX));
    }

    #[test]
    fn cmov_test() {

        let insts: [u8; 2 * 10 + 3 * 2] = [
            // IRMOVQ $9 $rax
            0x30, 0xF0, 0x09, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $4 $rbx
            0x30, 0xF3, 0x04, 0, 0, 0, 0, 0, 0, 0, // subq rax rbx
            0x61, 0x03, // -> non-zero
            // cmove rax rcx
            0x23, 0x01, // cmovne rax rdx
            0x24, 0x02,
        ];
        let mut ram = Ram::new(MEM_SIZE);
        let mut machine = Cpu::new(0, 0);
        ram.load(0, &insts);
        machine.start(&mut ram);
        assert_eq!(0, machine.get_register(Y8R::RCX));
        assert_eq!(9, machine.get_register(Y8R::RDX));

        let insts: [u8; 2 * 10 + 3 * 2] = [
            // IRMOVQ $9 $rax
            0x30, 0xF0, 0x09, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $9 $rbx
            0x30, 0xF3, 0x09, 0, 0, 0, 0, 0, 0, 0, // subq rax rbx
            0x61, 0x03, // -> zero
            // cmove rax rcx
            0x23, 0x01, // cmovne rax rdx
            0x24, 0x02,
        ];
        let mut ram = Ram::new(MEM_SIZE);
        let mut machine = Cpu::new(0, 0);
        ram.load(0, &insts);
        machine.start(&mut ram);
        assert_eq!(9, machine.get_register(Y8R::RCX));
        assert_eq!(0, machine.get_register(Y8R::RDX));
    }
}
