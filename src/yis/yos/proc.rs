use crate::{yis::{inst::Y8R, cpu::Cpu, ram::Ram}, ycc::INIT_SP};

pub struct Proc {
    pid: u32,
    context: Context,
    state: ProcState,
    mem_base: usize,
    mem_bound: usize,
}

impl Proc {
    pub fn new(pid: u32, mem_base: usize, mem_bound: usize) -> Proc {
        Proc {
            pid,
            context: Context {
                rax: 0,
                rbx: 0,
                rcx: 0,
                rdx: 0,
                rsp: INIT_SP,
                rpb: 0,
                pc: 0x1000, // FIXME: 0x1000 is equal to INIT_POS in app.rs
            },
            state: ProcState::Ready,
            mem_base,
            mem_bound,
        }
    }

    pub fn get_pid(&self) -> u32 {
        self.pid
    }

    pub fn is_ready(&self) -> bool {
        self.state == ProcState::Ready
    }

    pub fn go_running(&mut self, cpu: &mut Cpu, ram: &mut Ram) {
        self.state = ProcState::Running;
        cpu.set_register(Y8R::RAX, self.context.rax);
        cpu.set_register(Y8R::RBX, self.context.rbx);
        cpu.set_register(Y8R::RCX, self.context.rcx);
        cpu.set_register(Y8R::RDX, self.context.rdx);
        cpu.set_register(Y8R::RSP, self.context.rsp);
        cpu.set_register(Y8R::RBP, self.context.rpb);
        cpu.set_pc(self.context.pc);
        ram.set_base_bound(self.mem_base, self.mem_bound);
    }

    pub fn go_ready(&mut self, cpu: &Cpu, ram: &Ram) {
        self.state = ProcState::Ready;
        self.context.rax = cpu.get_register(Y8R::RAX);
        self.context.rbx = cpu.get_register(Y8R::RBX);
        self.context.rcx = cpu.get_register(Y8R::RCX);
        self.context.rdx = cpu.get_register(Y8R::RDX);
        self.context.rsp = cpu.get_register(Y8R::RSP);
        self.context.rpb = cpu.get_register(Y8R::RBP);
        self.context.pc = cpu.get_pc();
        (self.mem_base, self.mem_bound) = ram.get_base_bound();
    }

    pub fn exit(&mut self) {
        self.state = ProcState::Terminated;
    }
}

struct Context {
    rax: u64,
    rbx: u64,
    rcx: u64,
    rdx: u64,
    rsp: u64,
    rpb: u64,
    pc: usize,
}

#[derive(PartialEq)]
enum ProcState {
    Ready,
    Running,
    // Blocked,
    Terminated,
}