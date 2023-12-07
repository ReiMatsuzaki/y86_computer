use crate::yis::inst::Y8R;

use super::{proc::SeqProcessor, ram::Ram, inst::Y8S, console::Console};

const MAX_CYCLE: u64 = 10000;
pub struct Computer {
    proc: SeqProcessor,
    ram: Ram,
    console: Console,
    verbose: i64,
    watch_memory_range: Option<(usize, usize)>,
}

impl Computer {
    pub fn new(mem_size: usize, verbose: i64, watch_memory_range: Option<(usize, usize)>) -> Computer {
        Computer {
            proc: SeqProcessor::new(verbose),
            ram: Ram::new(mem_size),
            verbose,
            watch_memory_range,
            console: Console::new(),
        }
    }

    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        self.ram.load(pos, insts);
    }

    pub fn start(&mut self) -> Option<(u64, u64)> {
        for cyc in 0..MAX_CYCLE {
            let stat = self.proc.cycle(&mut self.ram);
            self.console.cycle(&mut self.ram);

            if self.verbose >= 2 {
                self.proc.print_registers();
                self.print_stack();
                self.print_ram();
                println!("");
            }

            if stat == Y8S::HLT {
                let rax = self.proc.get_register(Y8R::RAX);
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
        let mini = self.proc.get_register(Y8R::RSP) / 8;
        println!("stack:");
        for i in mini..(init_sp / 8) {
            let addr = init_sp - (i - mini + 1) * 8;
            let x = self.ram.read_quad(addr as usize);
            print!("{0:>04X} : {1:X} ", addr, x);
            if addr == self.proc.get_register(Y8R::RBP) {
                print!(" <- rbp");
            }
            if addr == self.proc.get_register(Y8R::RSP) {
                print!(" <- rsp");
            }
            println!("");
        }
    }

}
