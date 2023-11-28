pub mod utils {
    pub fn print_bytes(xs: &Vec<u8>) {
        let maxj = xs.len() / 16;
        for j in 0..maxj {
            print!("{0:<2}: ", j);
            for i in 0..16 {
                let addr = 16 * j + i;
                if addr < xs.len() {
                    print!("{0:>02X} ", xs[16 * j + i]);
                }
            }
            println!("");
        }
    }
}

// Y86_64 simulator
#[derive(Debug, PartialEq, Clone)]
pub enum Y8R {
    RAX = 0x0,
    RCX = 0x1,
    RDX = 0x2,
    RBX = 0x3,
    RSP = 0x4,
    RSI = 0x5,
    RDI = 0x6,
    R8 = 0x7,
    R9 = 0x8,
    R10 = 0x9,
    R11 = 0xA,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Y8S {
    AOK = 0x1,
    HLT = 0x2,
    ADR = 0x3,
    INS = 0x4,
}

#[derive(Debug, Clone, Copy)]
pub enum OpqFn {
    ADD,
    SUB,
    AND,
    OR,
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

pub fn encode_codefn(code_fn: CodeFn) -> u8 {
    match code_fn {
        CodeFn::HALT => 0x00,
        CodeFn::NOP => 0x10,
        CodeFn::RRMOVQ => 0x20,
        CodeFn::IRMOVQ => 0x30,
        CodeFn::RMMOVQ => 0x40,
        CodeFn::MRMOVQ => 0x50,
        CodeFn::OPQ(f) => {
            0x60 + match f {
                OpqFn::ADD => 0x00,
                OpqFn::SUB => 0x01,
                OpqFn::AND => 0x02,
                OpqFn::OR => 0x03,
            }
        }
        CodeFn::JXX(JxxFn::JE) => 0x73,
        CodeFn::JXX(JxxFn::JNE) => 0x74,
        CodeFn::CALL => 0x80,
        CodeFn::RET => 0x90,
        CodeFn::PUSHQ => 0xA0,
        CodeFn::POPQ => 0xB0,
    }
}
pub fn decode_codefn(x: u8) -> Option<CodeFn> {
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
        0x73 => Some(CodeFn::JXX(JxxFn::JE)),
        0x74 => Some(CodeFn::JXX(JxxFn::JNE)),
        0x80 => Some(CodeFn::CALL),
        0x90 => Some(CodeFn::RET),
        0xA0 => Some(CodeFn::PUSHQ),
        0xB0 => Some(CodeFn::POPQ),
        _ => None,
    }
}
pub fn split_byte(x: u8) -> (u8, u8) {
    return (x >> 4, x & 0x0F);
}
pub fn read_as_words(memory: &[u8], addr: usize) -> u64 {
    let mut x: u64 = 0;
    for i in 0..8 {
        let m = memory[addr + i] as u64;
        x += m << 8 * i;
    }
    x
}
pub fn write_words(memory: &mut [u8], addr: usize, x: u64) {
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
        let val_c: u64 = self.memory[c0].into();
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
    pub fn cycle(&mut self) {
        let fetched = self.fetch();
        if self.verbose >= 2 {
            println!("{:?}", fetched);
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
            println!(
                "pc=0x{0:X}, RAX=0x{1:X}, RBX=0x{2:X}, RCX=0x{3:X}, RDX=0x{4:X}, RSP=0x{5:X}, ZF={6}",
                self.pc,
                self.regs[Y8R::RAX as usize],
                self.regs[Y8R::RBX as usize],
                self.regs[Y8R::RCX as usize],
                self.regs[Y8R::RDX as usize],
                self.regs[Y8R::RSP as usize],
                self.zf,
            );
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
                println!("machine halt.");
                return;
            }
        }
    }
    pub fn get_memory(&self, i: usize) -> u8 {
        return self.memory[i];
    }
    pub fn get_register(&self, r: Y8R) -> u64 {
        return self.regs[r as usize];
    }
}
pub fn make_machine(verbose: i64) -> SeqProcessor {
    let machine = SeqProcessor {
        regs: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        pc: 0,
        zf: 0,
        sf: 0,
        of: 0,
        memory: [0; 1024],
        stat: Y8S::AOK,
        verbose,
    };
    return machine;
}
