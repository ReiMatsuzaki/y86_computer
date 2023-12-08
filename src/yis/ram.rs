pub struct Ram {
    // represent cashed memory
    byte_array: ByteArray,
    tag_bits: usize,
    set_bits: usize,
    offset_bits: usize,
    cache_lines: Vec<CacheLine>,
    write_mode: CacheWriteMode,
}

enum CacheWriteMode {
    WriteBack,
    WriteThrough,
}

struct CacheLine {
    valid: bool,
    tag: u64,
    data: Vec<u8>,
}

impl Ram {
    pub fn new(mem_size: usize) -> Ram {
        Ram {
            byte_array: ByteArray::new(mem_size),
            tag_bits: 0,
            set_bits: 0,
            offset_bits: 0,
            cache_lines: Vec::new(),
            write_mode: CacheWriteMode::WriteBack,
        }
    }

    pub fn read(&self, addr: usize) -> u8 {
        self.byte_array.read(addr)
    }

    pub fn write(&mut self, addr: usize, value: u8) {
        self.byte_array.write(addr, value);
    }

    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        for i in 0..insts.len() {
            self.write(pos + i, insts[i]);
        }
    }

    pub fn size(&self) -> usize {
        self.byte_array.size()
    }

    pub fn read_quad(&self, addr: usize) -> u64 {
        let mut xs: [u8; 8] = [0; 8];
        for i in 0..8 {
            xs[i] = self.read(addr + i);
        }
        read_as_words(&xs, 0)
    }

    pub fn write_quad(&mut self, addr: usize, x: u64) {
        let mut xs: [u8; 8] = [0; 8];
        write_words(&mut xs, 0, x);
        for i in 0..8 {
            self.write(addr + i, xs[i]);
        }
    }

    pub fn print(&self, start: Option<usize>, end: Option<usize>) {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(self.size());
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
                    print!("{0:>02X} ", self.read(16 * j + i));
                }
            }
            println!("");
        }
    }    
}

impl ByteArray {
    pub fn new(mem_size: usize) -> ByteArray {
        ByteArray {
            memory: vec![0; mem_size],
        }
    }

    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        for i in 0..insts.len() {
            self.memory[pos + i] = insts[i];
        }
    }

    pub fn size(&self) -> usize {
        self.memory.len()
    }

    pub fn read(&self, addr: usize) -> u8 {
        self.memory[addr]
    }

    pub fn write(&mut self, addr: usize, x: u8) {
        self.memory[addr] = x;
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

pub struct ByteArray {
    pub memory: Vec<u8>,
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
