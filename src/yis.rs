use csapp::utils::print_bytes;
use std::fmt;

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

fn get_register(x: u8) -> Option<Y8R> {
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
    CALL,
    RET,
    PUSHQ,
    POPQ,
}

fn decode_codefn(x: u8) -> Option<CodeFn> {
    match x {
        0x00 => Some(CodeFn::HALT),
        0x10 => Some(CodeFn::NOP),
        0x20 => Some(CodeFn::RRMOVQ),
        0x30 => Some(CodeFn::IRMOVQ),
        0x40 => Some(CodeFn::RMMOVQ),
        0x50 => Some(CodeFn::MRMOVQ),

        0x60 => Some(CodeFn::OPQ(OpqFn::ADD)),
        0x61 => Some(CodeFn::OPQ(OpqFn::SUB)),
        0x62 => Some(CodeFn::OPQ(OpqFn::AND)),
        0x63 => Some(CodeFn::OPQ(OpqFn::OR)),
        0x64 => Some(CodeFn::OPQ(OpqFn::MUL)),
        0x65 => Some(CodeFn::OPQ(OpqFn::DIV)),


        0x73 => Some(CodeFn::JXX(JxxFn::JE)),
        0x74 => Some(CodeFn::JXX(JxxFn::JNE)),
        0x80 => Some(CodeFn::CALL),
        0x90 => Some(CodeFn::RET),
        0xA0 => Some(CodeFn::PUSHQ),
        0xB0 => Some(CodeFn::POPQ),
        _ => None,
    }
}

fn split_byte(x: u8) -> (u8, u8) {
    return (x >> 4, x & 0x0F);
}

fn read_as_words(memory: &[u8], addr: usize) -> u64 {
    let mut x: u64 = 0;
    for i in 0..8 {
        let m = memory[addr + i] as u64;
        x += m << 8 * i;
    }
    x
}

fn write_words(memory: &mut [u8], addr: usize, x: u64) {
    let xs = x.to_be_bytes();
    for i in 0..8 {
        memory[addr + 7 - i] = xs[i];
    }
}

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
            match get_register(r) {
                Some(r) => format!("{:?}", r),
                None => "None".to_string(),
            }
        }
        let ra = g(self.ra);
        let rb = g(self.rb);
        write!(f, "code_fn= {:?}, ra: {1:?}, rb: {2:?}, c: 0x{3:X}", 
               self.code_fn, ra, rb, self.val_c)
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

pub struct SeqProcessor {
    regs: [u64; 16],
    pc: usize,
    zf: u8,
    sf: u8,
    of: u8,
    memory: [u8; 1024],
    stat: Y8S,

    verbose: i64,
    watch_memory_range: Option<(usize, usize)>,
}

impl SeqProcessor {
    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        for i in 0..insts.len() {
            self.memory[pos + i] = insts[i];
        }
    }
    fn fetch(&mut self) -> Fetched {
        self.stat = Y8S::AOK;

        let code_fn = match decode_codefn(self.memory[self.pc]) {
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

        let (ra, rb) = split_byte(self.memory[self.pc + 1]);
        let (val_p, c0): (usize, usize) = match code_fn {
            CodeFn::HALT => (self.pc + 1, self.pc + 2),
            CodeFn::NOP => (self.pc + 1, self.pc + 2),
            CodeFn::RRMOVQ => (self.pc + 2, self.pc + 2),
            CodeFn::IRMOVQ => (self.pc + 10, self.pc + 2),
            CodeFn::RMMOVQ => (self.pc + 10, self.pc + 2),
            CodeFn::MRMOVQ => (self.pc + 10, self.pc + 2),
            CodeFn::OPQ(_) => (self.pc + 2, self.pc + 2),
            CodeFn::JXX(_) => (self.pc + 9, self.pc + 1),
            CodeFn::CALL => (self.pc + 9, self.pc + 1),
            CodeFn::RET => (self.pc + 1, self.pc + 1),
            CodeFn::PUSHQ => (self.pc + 2, self.pc + 2),
            CodeFn::POPQ => (self.pc + 2, self.pc + 2),
        };
        // let val_c: u64 = self.memory[c0].into();
        let val_c: u64 = read_as_words(&self.memory, c0);
        return Fetched {
            code_fn,
            ra,
            rb,
            val_c,
            val_p,
        };
    }
    fn decode(&self, fetched: &Fetched) -> Decoded {
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
            CodeFn::CALL => (rsp, 0xF),
            CodeFn::RET => (rsp, 0xF),
            CodeFn::PUSHQ => (rsp, 0xF),
            CodeFn::POPQ => (rsp, ra),
        };
        let val_a: u64 = self.regs[src_a];
        let val_b: u64 = self.regs[src_b];
        return Decoded {
            val_a,
            val_b,
            dst_e,
            dst_m,
        };
    }
    fn execute(&mut self, f: &Fetched, d: &Decoded) -> Executed {
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
                OpqFn::ADD => va + vb,
                OpqFn::SUB => !(va + !vb), // b-a
                OpqFn::AND => va & vb,
                OpqFn::OR => va | vb,
                OpqFn::MUL => va * vb,
                OpqFn::DIV => vb / va,
            },
            CodeFn::JXX(_) => 0,
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
        return Executed { val_e };
    }
    fn memory(&mut self, f: &Fetched, d: &Decoded, e: &Executed) -> Memoried {
        let val_m = match f.code_fn {
            CodeFn::HALT => 0,
            CodeFn::NOP => 0,
            CodeFn::RRMOVQ => 0,
            CodeFn::IRMOVQ => 0,
            CodeFn::RMMOVQ => {
                let addr = e.val_e as usize;
                write_words(&mut self.memory, addr, d.val_a);
                0
            }
            CodeFn::MRMOVQ => {
                let addr = e.val_e as usize;
                read_as_words(&self.memory, addr)
            }
            CodeFn::OPQ(_) => 0,
            CodeFn::JXX(_) => 0,
            CodeFn::CALL => {
                let addr = e.val_e as usize;
                write_words(&mut self.memory, addr, f.val_p as u64);
                0
            }
            CodeFn::PUSHQ => {
                let addr = e.val_e as usize;
                write_words(&mut self.memory, addr, d.val_a as u64);
                0
            }
            CodeFn::RET | CodeFn::POPQ => {
                let addr = d.val_a as usize;
                read_as_words(&self.memory, addr)
            }
        };
        return Memoried { val_m };
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
        println!("pc=0x{0:0>2X}", self.pc);
        println!("RAX=0x{0:X}, RBX=0x{1:X}, RCX=0x{2:X}, RDX=0x{3:X}, RSP=0x{4:X}, RBP=0x{5:X}, RSI=0x{6:X}, RDI=0x{7:X}",
            self.get_register(Y8R::RAX),
            self.get_register(Y8R::RBX),
            self.get_register(Y8R::RCX),
            self.get_register(Y8R::RDX),
            self.get_register(Y8R::RSP),
            self.get_register(Y8R::RBP),
            self.get_register(Y8R::RSI),
            self.get_register(Y8R::RDI),
        );
        println!(" R8=0x{0:X},  R9=0x{1:X}, R10=0x{2:X}, R11=0x{3:X}",
            self.regs[Y8R::R8 as usize],
            self.regs[Y8R::R9 as usize],
            self.regs[Y8R::R10 as usize],
            self.regs[Y8R::R11 as usize],
        );
    }
    pub fn cycle(&mut self) {
        let fetched = self.fetch();
        if self.verbose >= 1 {
            println!("fetched:\n{}", fetched);
        }
        let decoded = self.decode(&fetched);
        if self.verbose >= 2 {
            println!("{:?}", decoded);
        }
        let executed = self.execute(&fetched, &decoded);
        if self.verbose >= 2 {
            println!("{:?}", executed);
        }
        let memoried = self.memory(&fetched, &decoded, &executed);
        if self.verbose >= 2 {
            println!("{:?}", memoried);
        }
        self.write(&fetched, &decoded, &executed, &memoried);
        if self.verbose >= 1 {
            self.print_registers();
            if let Some((s, e)) = self.watch_memory_range {
                let x = &self.memory[s..e];
                print_bytes(&Vec::from(x));
            }
            println!("");
        }
        if self.verbose > 2 {
            for j in 0..10 {
                print!("{0:<2}: ", j);
                for i in 0..16 {
                    print!("{0:>02X} ", self.memory[16 * j + i]);
                }
                println!("");
            }
        }
    }
    pub fn start(&mut self) {
        loop {
            self.cycle();
            if self.stat == Y8S::HLT {
                break;
            }
        }
    }
    // pub fn get_memory(&self, i: usize) -> u8 {
    //     return self.memory[i];
    // }
    pub fn get_register(&self, r: Y8R) -> u64 {
        return self.regs[r as usize];
    }
}

pub fn make_machine(verbose: i64, watch_memory_range: Option<(usize, usize)>) -> SeqProcessor {
    let machine = SeqProcessor {
        regs: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        pc: 0,
        zf: 0,
        sf: 0,
        of: 0,
        memory: [0; 1024],
        stat: Y8S::AOK,
        verbose,
        watch_memory_range,
    };
    return machine;
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn split_byte_test() {
      let x: u8 = 0xAB;
      let (y, z) = split_byte(x);
      assert_eq!(0x0A, y);
      assert_eq!(0x0B, z);
  }

  #[test]
  fn seq_processor_test() {
      let mut machine = make_machine(0, None);
      let insts: [u8; 4*10 + 2*9 + 3*2 + 2*1] = [
          // 0x00: IRMOVQ $9  $rdx
          0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0,
          // 0x0A: IRMOVQ $21 $rbx
          0x30, 0xF3, 0x15, 0, 0, 0, 0, 0, 0, 0, 
          // 0x14: subq rdx rbx
          0x61, 0x23, 
          // -> rdx=0x09, rbx=0x0C
          // 0x16: IRMOVQ $128 $rsp
          0x30, 0xF4, 0x80, 0, 0, 0, 0, 0, 0, 0, 
          // -> rsp=0x80
          // 0x20: RMMOVQ $rsp 100(%rbx)
          0x40, 0x43, 0x64, 0, 0, 0, 0, 0, 0, 0, 
          // -> M[0x70]=0x80    // 0x64+0x0C=0x70
          // 0x02A: PUSHQ $rdx
          0xA0, 0x2F, 
          // -> M[0x78]=0x09    // 0x80-0x08=0x78
          // 0x2C: POPQ $rax
          0xB0, 0x0F, 
          // -> rax=0x09
          // 0x2E: je done
          0x73, 0x40, 0, 0, 0, 0, 0, 0, 0, 
          // 0x37: call proc
          0x80, 0x41, 0, 0, 0, 0, 0, 0, 0, 
          // 0x40: halt
          0x00, 
          // 0x41: ret
          0x90,
      ];
      machine.load(0, &insts);
      machine.start();
      assert_eq!(0x80, machine.memory[0x70]);
      assert_eq!(0x40, machine.memory[0x78]);
      assert_eq!(0x09, machine.get_register(Y8R::RAX));
  }  

  #[test]
  fn opq_test() {
    let mut machine = make_machine(0, None);
    let insts: [u8; 4*10 + 2*2] = [
        // IRMOVQ $9  $rdx
        0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0,
        // IRMOVQ $4 $rbx
        0x30, 0xF3, 0x04, 0, 0, 0, 0, 0, 0, 0, 
        // mulq rdx rbx
        0x64, 0x23, 
        // -> rdx=0x09, rbx=36=0x24

        // IRMOVQ $4  $rdx
        0x30, 0xF2, 0x04, 0, 0, 0, 0, 0, 0, 0,
        // IRMOVQ $9 $rax
        0x30, 0xF0, 0x09, 0, 0, 0, 0, 0, 0, 0, 
        // divq rdx rax
        0x65, 0x20, 
        // -> rax=0x02
    ];
    machine.load(0, &insts);
    machine.start();
    assert_eq!(0x24, machine.get_register(Y8R::RBX));
    assert_eq!(0x02, machine.get_register(Y8R::RAX));
  }
}