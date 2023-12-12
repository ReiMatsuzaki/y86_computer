use crate::yis::{inst::Y8R, ram::Ram};

use super::{super::{inst::{Exception, Y8S}, cpu::Cpu}, proc::Proc};

pub struct Kernel {
    proc_table: Vec<Proc>,
    current_pid: u32,
}

impl Kernel {
    pub fn new() -> Kernel {
        Kernel {
            proc_table: Vec::new(),
            current_pid: 0,
        }
    }

    pub fn current_proc(&self) -> &Proc {
        &self.proc_table[self.current_pid as usize]
    }

    pub fn add_proc(&mut self, mem_base: usize, mem_bound: usize) {
        let pid = self.proc_table.len() as u32;
        let proc = Proc::new(pid, mem_base, mem_bound);
        self.proc_table.push(proc);
    }

    fn switch(&mut self, cpu: &mut Cpu, ram: &mut Ram, pid1: u32, pid2: u32) {
        let proc1 = &mut self.proc_table[pid1 as usize];
        proc1.go_ready(cpu, ram);
        let proc2 = &mut self.proc_table[pid2 as usize];
        proc2.go_running(cpu, ram);
    }

    pub fn handle_exception(&mut self, e: Exception, cpu: &mut Cpu, ram: &mut Ram) -> Y8S {
        match e {
            Exception::DivideError => {
                println!("Kernel: divide by zero error found. Replace result with 1");
                cpu.set_register(Y8R::RAX, 1);
                Y8S::AOK
            },
            Exception::TimerInterrupt => {
                // FIXME: remove message.
                // FIXME: chose pid
                println!("Kernel: timer interrupt found. context switch pid:0->pid:1");
                self.switch(cpu, ram, 0, 1);
                Y8S::AOK
            },
            Exception::ProtectionFault => {
                println!("Kernel: protection fault found. Shutdown");
                Y8S::HLT
            },
        }
    }
}