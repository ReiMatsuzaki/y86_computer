use crate::yis::inst::Y8R;

use super::{cpu::{Cpu, Fetched}, ram::Ram, inst::{Y8S, Exception}, console::Console, yos::kernel::Kernel};

const MAX_CYCLE: u64 = 10000;

pub enum Watching {
    Reg(Y8R),
    Mem(usize),
}

pub struct Computer {
    cpu: Cpu,
    ram: Ram,
    console: Console,
    kernel: Kernel,
    verbose: i64,
    watch_memory_range: Option<(usize, usize)>,
}

impl Computer {
    pub fn new(mem_size: usize, verbose: i64, watch_memory_range: Option<(usize, usize)>) -> Computer {
        Computer {
            cpu: Cpu::new(),
            ram: Ram::new(mem_size),
            verbose,
            watch_memory_range,
            console: Console::new(),
            kernel: Kernel::new(),
        }
    }

    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        self.ram.load(pos, insts);
        let (base, bound) = self.ram.get_base_bound();
        self.kernel.add_proc(base, bound);
    }

    pub fn start(&mut self, watchings: &Vec<Watching>) -> Option<(u64, u64)> {
        if self.verbose >= 2 {
            self.print_header(watchings);
        }

        self.kernel.start(&mut self.cpu, &mut self.ram);
        for cyc in 0..MAX_CYCLE {
            if self.verbose >=2 {
                self.print_log_1(watchings);
            }

            // FIXME: clean code
            let (fetched, res_cpu) = self.cpu.cycle(&mut self.ram);
            let res_cpu = match res_cpu {
                Ok(s) => if cyc > 0 && cyc % 5 == 0 {
                        Err(Exception::TimerInterrupt)
                    } else {
                        Ok(s)
                    },
                Err(e) => Err(e)
            };
            self.console.cycle(&mut self.ram);

            if self.verbose >= 2 {
                self.print_log_2(&fetched);
            }

            let stat = match res_cpu {
                Ok(s) => s,
                Err(e) => {
                    self.kernel.handle_exception(e, &mut self.cpu, &mut self.ram)
                }
            };

            if stat == Y8S::HLT {
                let rax = self.cpu.get_register(Y8R::RAX);
                return Some((cyc, rax));
            }
        }
        None            
    }

    fn print_header(&self, watchings: &Vec<Watching>) {
        println!("");
        for watching in watchings {
            match watching {
                Watching::Reg(r) => {
                    let r = format!("{0:?}", r).to_ascii_lowercase();
                    print!(" %{0:<7}", r)
                },
                Watching::Mem(addr) => {
                    print!(" {0:>04X}     ", addr);
                },
            }
        }
        print!("pid    pc: code               ");
        println!("");
    }

    fn print_log_1(&self, watchings: &Vec<Watching>) {
        let running_pid = self.kernel.current_proc().get_pid();
        for watching in watchings {
            match watching {
                Watching::Reg(r) => {
                    let v = self.cpu.get_register(r.clone());
                    print!("{0:>5X}    ", v);
                },
                Watching::Mem(addr) => {
                    let v = self.ram.read_const_quad(*addr);
                    print!("{0:>5X}    ", v);
                },
            }
        }
        print!("{0:>3} {1:>5X}: ", running_pid, self.cpu.get_pc());
    }

    fn print_log_2(&self, fetched: &Fetched) {
        print!("{0}", fetched);
        println!("");
        if self.verbose >= 3 {
            self.print_stack();
            self.print_ram();
            println!("");
        }
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

    pub fn set_ram_base_bound(&mut self, base: usize, bound: usize) {
        self.ram.set_base_bound(base, bound);
    }

}
