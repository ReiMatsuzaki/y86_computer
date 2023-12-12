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

    pub fn start(&mut self, cpu: &mut Cpu, ram: &mut Ram) {
        let proc = &mut self.proc_table[self.current_pid as usize];
        proc.go_running(cpu, ram);
    }

    pub fn current_proc(&self) -> &Proc {
        &self.proc_table[self.current_pid as usize]
    }

    pub fn add_proc(&mut self, mem_base: usize, mem_bound: usize) {
        let pid = self.proc_table.len() as u32;
        let proc = Proc::new(pid, mem_base, mem_bound);
        self.proc_table.push(proc);
    }

    fn switch(&mut self, cpu: &mut Cpu, ram: &mut Ram, next_pid: u32) {
        let proc1 = &mut self.proc_table[self.current_pid as usize];
        proc1.go_ready(cpu, ram);
        let next_proc = &mut self.proc_table[next_pid as usize];
        next_proc.go_running(cpu, ram);
        self.current_pid = next_pid;
    }

    fn get_next_pid(&self) -> Option<u32> {
        // FIXME: more sophisticated scheduling
        self.proc_table.iter()
            .find(|proc| proc.is_ready())
            .map(|proc| proc.get_pid())
    }

    pub fn handle_exception(&mut self, e: Exception, cpu: &mut Cpu, ram: &mut Ram) -> Y8S {
        match e {
            Exception::DivideError => {
                println!("==== Kernel: divide by zero error found. Replace result with 1 ====");
                cpu.set_register(Y8R::RAX, 1);
                Y8S::AOK
            },
            Exception::TimerInterrupt => {
                println!("===== Kernel: timer interrupt found. context switch. ====");
                if let Some(next_pid) = self.get_next_pid() {
                    self.switch(cpu, ram, next_pid);
                }
                Y8S::AOK
            },
            Exception::ProtectionFault => {
                println!("==== Kernel: protection fault found. Shutdown ====");
                Y8S::HLT
            },
        }
    }
}