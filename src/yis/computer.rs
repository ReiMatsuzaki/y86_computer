use crate::yis::inst::Y8R;

use super::{cpu::Cpu, ram::Ram, inst::Y8S, console::Console, kernel::Kernel};

const MAX_CYCLE: u64 = 10000;
pub struct Computer {
    cpu: Cpu,
    ram: Ram,
    console: Console,
    kernel: Kernel,
    verbose: i64,
    watch_memory_range: Option<(usize, usize)>,
}

impl Computer {
    pub fn new(mem_size: usize, pc: usize, verbose: i64, watch_memory_range: Option<(usize, usize)>) -> Computer {
        Computer {
            cpu: Cpu::new(verbose, pc),
            ram: Ram::new(mem_size),
            verbose,
            watch_memory_range,
            console: Console::new(),
            kernel: Kernel::new(),
        }
    }

    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        self.ram.load(pos, insts);
    }

    pub fn start(&mut self) -> Option<(u64, u64)> {
        for cyc in 0..MAX_CYCLE {
            let res_cpu = self.cpu.cycle(&mut self.ram);
            self.console.cycle(&mut self.ram);
            let stat = match res_cpu {
                Ok(s) => s,
                Err(e) => {
                    self.kernel.handle_exception(e, &mut self.cpu)
                }
            };

            if self.verbose >= 2 {
                self.cpu.print_registers();
                self.print_stack();
                self.print_ram();
                println!("");
            }

            if stat == Y8S::HLT {
                let rax = self.cpu.get_register(Y8R::RAX);
                return Some((cyc, rax));
            }
        }
        None            
    }

    pub fn print_ram(&self) {
        if let Some((s, e)) = self.watch_memory_range {
            self.ram.print(Some(s), Some(e));
        }
    }

    fn print_stack(&self) {
        let init_sp = crate::ycc::INIT_SP;
        let mini = self.cpu.get_register(Y8R::RSP) / 8;
        println!("stack:");
        for i in mini..(init_sp / 8) {
            let addr = init_sp - (i - mini + 1) * 8;
            let x = self.ram.read_const_quad(addr as usize);
            print!("{0:>04X} : {1:X} ", addr, x);
            if addr == self.cpu.get_register(Y8R::RBP) {
                print!(" <- rbp");
            }
            if addr == self.cpu.get_register(Y8R::RSP) {
                print!(" <- rsp");
            }
            println!("");
        }
    }

}
