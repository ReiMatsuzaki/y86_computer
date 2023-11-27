#[derive(Debug, Clone, Copy)]
enum OpqFn {
    ADD,
    SUB,
    AND,
    OR,
}
#[derive(Debug, Clone, Copy)]
enum JxxFn {
    JE,
    JNE,
}
#[derive(Debug, Clone, Copy)]
enum CmovFn {}
#[derive(Debug, Clone, Copy)]
enum CodeFn {
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
fn encode_codefn(code_fn: CodeFn) -> u8 {
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
        0x73 => Some(CodeFn::JXX(JxxFn::JE)),
        0x74 => Some(CodeFn::JXX(JxxFn::JNE)),
        0x80 => Some(CodeFn::CALL),
        0x90 => Some(CodeFn::RET),
        0xA0 => Some(CodeFn::PUSHQ),
        0xB0 => Some(CodeFn::POPQ),
        _ => None,
    }
}
fn split(x: u8) -> (u8, u8) {
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
#[derive(Debug, PartialEq, Clone)]
enum Y8Reg {
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
enum Y8S {
    AOK = 0x1,
    HLT = 0x2,
    ADR = 0x3,
    INS = 0x4,
}
struct Machine {
    regs: [u64; 16],
    pc: usize,
    zf: u8,
    sf: u8,
    of: u8,
    memory: [u8; 1024],
    stat: Y8S,
    verbose: i64,
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
    code_fn: CodeFn,
    val_p: usize,
    val_c: u64,

    val_a: u64,
    val_b: u64,
    dst_e: usize,
    dst_m: usize,
}
#[derive(Debug, Clone)]
struct Executed {
    code_fn: CodeFn,
    val_a: u64,
    val_c: u64,
    val_p: usize,
    dst_e: usize,
    dst_m: usize,

    val_e: u64,
}
#[derive(Debug)]
struct Memoried {
    code_fn: CodeFn,
    val_c: u64,
    val_p: usize,
    dst_e: usize,
    val_e: u64,
    dst_m: usize,
    val_m: u64,
}
impl Machine {
    fn load(&mut self, pos: usize, insts: &[u8]) {
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

        let (ra, rb) = split(self.memory[self.pc + 1]);
        let (val_p, c0): (usize, usize) = match code_fn {
            CodeFn::HALT => (self.pc + 1, self.pc+2),
            CodeFn::NOP => (self.pc + 1, self.pc+2),
            CodeFn::RRMOVQ => (self.pc + 2, self.pc+2),
            CodeFn::IRMOVQ => (self.pc + 10, self.pc+2),
            CodeFn::RMMOVQ => (self.pc + 10, self.pc+2),
            CodeFn::MRMOVQ => (self.pc + 10, self.pc+2),
            CodeFn::OPQ(_) => (self.pc + 2, self.pc+2),
            CodeFn::JXX(_) => (self.pc + 9, self.pc+1),
            CodeFn::CALL => (self.pc + 9, self.pc+1),
            CodeFn::RET => (self.pc + 1, self.pc+1),
            CodeFn::PUSHQ => (self.pc + 2, self.pc+2),
            CodeFn::POPQ => (self.pc + 2, self.pc+2),
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
    fn decode(&self, fetched: Fetched) -> Decoded {
        let ra = fetched.ra as usize;
        let rb = fetched.rb as usize;
        let rsp = Y8Reg::RSP as usize;
        let (src_a, src_b) = match fetched.code_fn {
            CodeFn::PUSHQ => (ra, rsp),
            CodeFn::POPQ => (rsp, rsp),
            CodeFn::CALL => (0xF, rsp),
            CodeFn::RET => (rsp, rsp),
            _ => (ra, rb)
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
            code_fn: fetched.code_fn,
            val_p: fetched.val_p,
            val_c: fetched.val_c,
            val_a,
            val_b,
            dst_e,
            dst_m,
        };
    }
    fn execute(&mut self, decoded: Decoded) -> Executed {
        let va = decoded.val_a;
        let vb = decoded.val_b;
        let vc = decoded.val_c;
        let val_e: u64 = match decoded.code_fn {
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
        match decoded.code_fn {
            CodeFn::OPQ(_) => {
                self.zf = if val_e == 0 { 0x1 } else { 0x0 };
                self.sf = if val_e > !val_e { 0x1 } else { 0x0 };
                self.of = 0x0; // FIXME
            }
            _ => {}
        }
        return Executed {
            code_fn: decoded.code_fn.clone(),
            val_a: decoded.val_a,
            val_c: decoded.val_c,
            val_p: decoded.val_p,
            dst_e: decoded.dst_e,
            dst_m: decoded.dst_m,

            val_e,
        };
    }
    fn memory(&mut self, executed: Executed) -> Memoried {
        let val_m = match executed.code_fn {
            CodeFn::HALT => 0,
            CodeFn::NOP => 0,
            CodeFn::RRMOVQ => 0,
            CodeFn::IRMOVQ => 0,
            CodeFn::RMMOVQ => {
                let addr = executed.val_e as usize;
                write_words(&mut self.memory, addr, executed.val_a);
                0
            },
            CodeFn::MRMOVQ => {
                let addr = executed.val_e as usize;
                read_as_words(&self.memory, addr)
            }
            CodeFn::OPQ(_) => 0,
            CodeFn::JXX(_) => 0,
            CodeFn::CALL => {
                let addr = executed.val_e as usize;
                write_words(&mut self.memory, addr, executed.val_p as u64);
                0
            }
            CodeFn::PUSHQ => {
                let addr = executed.val_e as usize;
                write_words(&mut self.memory, addr, executed.val_a as u64);
                0                
            }
            CodeFn::RET | CodeFn::POPQ => {
                let addr = executed.val_a as usize;
                read_as_words(&self.memory, addr)
            }
        };
        return Memoried {
            code_fn: executed.code_fn,
            val_c: executed.val_c,
            dst_e: executed.dst_e,
            val_e: executed.val_e,
            val_p: executed.val_p,
            dst_m: executed.dst_m,

            val_m,
        };
    }
    fn write(&mut self, memoried: Memoried) {
        self.regs[memoried.dst_e as usize] = memoried.val_e;
        self.regs[memoried.dst_m as usize] = memoried.val_m;
        let val_c = memoried.val_c as usize;
        let val_p = memoried.val_p;
        let val_m = memoried.val_m as usize;
        self.pc = match memoried.code_fn {
            CodeFn::CALL => val_c,
            CodeFn::RET => val_m,
            CodeFn::JXX(j) => {
                let jump = match j {
                    JxxFn::JE => self.zf==1,
                    JxxFn::JNE => self.zf==0
                };
                if jump {val_c} else {val_p}
            }
            _ => memoried.val_p
        };
    }
    fn cycle(&mut self) {
        let fetched = self.fetch();
        if self.verbose >= 2 {
            println!("{:?}", fetched);
        }
        let decoded = self.decode(fetched);
        if self.verbose >= 2 {
            println!("{:?}", decoded);
        }
        let executed = self.execute(decoded);
        if self.verbose >= 2 {
            println!("{:?}", executed);
        }
        let memoried = self.memory(executed);
        if self.verbose >= 2 {
            println!("{:?}", memoried);
        }
        self.write(memoried);
        if self.verbose >= 1 {
            println!(
                "pc=0x{0:X}, RAX=0x{1:X}, RBX=0x{2:X}, RCX=0x{3:X}, RDX=0x{4:X}, RSP=0x{5:X}, ZF={6}",
                self.pc,
                self.regs[Y8Reg::RAX as usize],
                self.regs[Y8Reg::RBX as usize],
                self.regs[Y8Reg::RCX as usize],
                self.regs[Y8Reg::RDX as usize],
                self.regs[Y8Reg::RSP as usize],
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
    fn start(&mut self) {
        loop {
            self.cycle();
            if self.stat == Y8S::HLT {
                println!("machine halt.");
                return;
            }
        }
    }
}
fn make_machine(verbose: i64) -> Machine {
    let machine = Machine {
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

fn test2() {
    let mut machine = make_machine(3);
    let insts: [u8; 4*10 + 2*9 + 3*2 + 2*1] = [
        0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $9  $rdx
        0x30, 0xF3, 0x15, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $21 $rbx
        0x61, 0x23, // subq rdx rbx
        // 0x61, 0x33, // subq rbx rbx
        0x30, 0xF4, 0x80, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $128 $rsp
        0x40, 0x43, 0x64, 0, 0, 0, 0, 0, 0, 0, // RMMOVQ $rsp 100(%rbx)
        0xA0, 0x2F, // PUSHQ $rdx
        0xB0, 0x0F, // POPQ $rax
        0x73, 0x40, 0, 0, 0, 0, 0, 0, 0, // je done
        0x80, 0x41, 0, 0, 0, 0, 0, 0, 0, // call proc
        0x00, // halt
        0x90, // ret
    ];
    machine.load(0, &insts);
    machine.start();
}
fn test1() {
    let mut machine = make_machine(2);
    let insts: [u8; 27] = [
        0x30, 0xF0, 11, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ RAX $11
        0x30, 0xF3, 9, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ RBX $9
        0x20, 0x31, // RRMOVQ RBX RCX
        0x10, // nop
        0x60, 0x13, // ADDQ RCX RBX
        0x10, // nop
        0x00, // halt
    ];
    machine.load(0, &insts);
    machine.start();
}
fn main() {
    test2();
}
