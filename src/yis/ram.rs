pub struct Ram {
    pub memory: Vec<u8>,
}

impl Ram {
    pub fn new(mem_size: usize) -> Ram {
        Ram {
            memory: vec![0; mem_size],
        }
    }

    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        for i in 0..insts.len() {
            self.memory[pos + i] = insts[i];
        }
    }

    pub fn read(&self, addr: usize) -> u8 {
        self.memory[addr]
    }

    pub fn read_quad(&self, addr: usize) -> u64 {
        read_as_words(&self.memory, addr)
    }

    pub fn write_quad(&mut self, addr: usize, x: u64) {
        write_words(&mut self.memory, addr, x);
    }

    pub fn print(&self, start: Option<usize>, end: Option<usize>) {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(self.memory.len());
        let minj = start / 16;
        let maxj = 1 + end / 16;
        print!("    ");
        for i in 0..16 {
            print!(" {0:X} ", i);
        }
        println!("");
        print!("----");
        for _ in 0..16 {
            print!("---");
        }
        println!("");
        for j in minj..maxj {
            print!("{0:<2X}: ", j);
            for i in 0..16 {
                let addr = 16 * j + i;
                if (start <= addr) & (addr < end) {
                    print!("{0:>02X} ", self.memory[16 * j + i]);
                }
            }
            println!("");
        }
    }    
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
