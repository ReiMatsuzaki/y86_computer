pub struct Ram {
    memory: CashedMemory,
    base: usize,
    bound: usize,
}

impl Ram {
    pub fn new(mem_size: usize) -> Ram {
        Ram {
            memory: CashedMemory::new(mem_size),
            base: 0,
            bound: 0x1_0000,
        }
    }

    // pub fn set_base(&mut self, base: usize) {
    //     self.base = base;
    // }

    fn addr(&self, vaddr: usize) -> usize {
        assert!(vaddr <= self.bound);
        self.base + vaddr
    }

    pub fn read(&mut self, addr: usize) -> u8 {
        self.memory.read(self.addr(addr))
    }

    pub fn write(&mut self, addr: usize, value: u8) {
        self.memory.write(self.addr(addr), value);
    }

    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        self.memory.load(self.addr(pos), insts);
    }

    pub fn size(&self) -> usize {
        self.memory.size()
    }

    pub fn read_quad(&mut self, addr: usize) -> u64 {
        self.memory.read_quad(self.addr(addr))
    }

    pub fn read_const_quad(&self, addr: usize) -> u64 {
        self.memory.read_const_quad(self.addr(addr))
    }

    pub fn write_quad(&mut self, addr: usize, x: u64) {
        self.memory.write_quad(self.addr(addr), x);
    }

    pub fn print(&self, start: Option<usize>, end: Option<usize>) {
        self.memory.print(start, end);
    }
}

pub struct CashedMemory {
    // represent cashed memory
    byte_array: ByteArray,
    // tag_bits: usize,
    set_bits: usize,
    offset_bits: usize,
    cache_sets: Vec<CacheSet>,
    write_mode: CacheWriteMode,

    read_cache_hit: usize,
    read_cache_miss: usize,
}

enum CacheWriteMode {
    // WriteBack,
    WriteThrough,
}

#[derive(Clone)]
struct CacheSet {
    lines: Vec<CacheLine>,
}

#[derive(Clone)]
struct CacheLine {
    valid: bool,
    tag: u64,
    unused_count: usize,
    data: Vec<u8>,
}

impl CashedMemory {
    pub fn new(mem_size: usize) -> CashedMemory {
        let tag_bits = 48;
        let set_bits = 8;
        let line_per_set = 2;
        let offset_bits = 64 - tag_bits - set_bits;
        let data_len = (1u64 << (offset_bits as u32)) as usize;
        let set_len = (1u64 << (set_bits as u32)) as usize;
        CashedMemory {
            byte_array: ByteArray::new(mem_size),
            // tag_bits,
            set_bits,
            offset_bits,
            cache_sets: vec![CacheSet {
                lines: vec![CacheLine {
                    valid: false,
                    tag: 0,
                    unused_count: 0,
                    data: vec![0; data_len],
                }; line_per_set],
            }; set_len],
            write_mode: CacheWriteMode::WriteThrough,
            read_cache_hit: 0,
            read_cache_miss: 0,
        }
    }

    pub fn read(&mut self, addr: usize) -> u8 {
        let (t, s, offset) = self.split(addr);
        for line in &mut self.cache_sets[s as usize].lines {
            if line.valid {
                line.unused_count += 1;
            }
        }
        match self.cache_sets[s as usize].lines.iter_mut().find(|line| line.tag == t) {
            Some(line) if line.valid => {
                // cache hit
                self.read_cache_hit += 1;
                line.unused_count = 0;
                line.data[offset as usize]
            }
            _ => {
                // cache miss
                self.read_cache_miss += 1;
                let line = self.fetch(addr);
                line.data[offset as usize]
            }
        }
    }

    pub fn write(&mut self, addr: usize, value: u8) {
        match self.write_mode {
            CacheWriteMode::WriteThrough => {
                self.byte_array.write(addr, value);
            }
        }
        let (t, s, offset) = self.split(addr);
        let cache_set: &mut CacheSet = &mut self.cache_sets[s as usize];
        for i in 0..cache_set.lines.len() {
            if cache_set.lines[i].tag == t {
                // cache hit
                cache_set.lines[i].data[offset as usize] = value;
                return;
            }
        }
        // cache miss
        self.fetch(addr);
    }

    fn split(&self, addr: usize) -> (u64, u64, u64) {
        let addr = addr as u64;
        let mask_s = (1 << self.set_bits) - 1;
        let t = addr.wrapping_shr((self.set_bits + self.offset_bits) as u32);
        let s = mask_s & (addr.wrapping_shr(self.offset_bits as u32));
        let offset = addr & ((1u64 << self.offset_bits) - 1u64);
        (t, s, offset)
    }

    fn fetch(&mut self, addr: usize) -> &CacheLine {
        let (t, s, offset) = self.split(addr);
        let i =  match self.find_invalid_line_index(s) {
            Some(i) => i,
            None => self.find_lru_line_index(s),
        };
        let line = &mut self.cache_sets[s as usize].lines[i];
        line.valid = true;
        line.tag = t;
        let base_addr = addr - offset as usize;

        let data_len = Self::pow2(self.offset_bits) as usize;
        for o in 0..data_len {
            let a = base_addr + o;
            line.data[o] = self.byte_array.read(a);
        }

        line
    }

    fn pow2(x: usize) -> u64 {
        1u64 << (x as u32)
    }

    fn find_invalid_line_index(&self, s: u64) -> Option<usize> {
        let cache_set = &self.cache_sets[s as usize];
        for i in 0..cache_set.lines.len() {
            if !cache_set.lines[i].valid {
                return Some(i);
            }
        }
        None
    }

    fn find_lru_line_index(&self, s: u64) -> usize {
        let cache_set = &self.cache_sets[s as usize];
        cache_set.lines.iter().enumerate().fold(0, |acc, (i, line)| {
            if line.unused_count > cache_set.lines[acc].unused_count {
                i
            } else {
                acc
            }
        })
    }

    pub fn load(&mut self, pos: usize, insts: &[u8]) {
        for i in 0..insts.len() {
            self.write(pos + i, insts[i]);
        }
    }

    pub fn size(&self) -> usize {
        self.byte_array.size()
    }

    pub fn read_quad(&mut self, addr: usize) -> u64 {
        let mut xs: [u8; 8] = [0; 8];
        for i in 0..8 {
            xs[i] = self.read(addr + i);
        }
        read_as_words(&xs, 0)
    }

    pub fn read_const_quad(&self, addr: usize) -> u64 {
        self.byte_array.read_quad(addr)
    }

    pub fn write_quad(&mut self, addr: usize, x: u64) {
        let mut xs: [u8; 8] = [0; 8];
        write_words(&mut xs, 0, x);
        for i in 0..8 {
            self.write(addr + i, xs[i]);
        }
    }

    pub fn print(&self, start: Option<usize>, end: Option<usize>) {
        self.byte_array.print(start, end);
    }    
}

#[cfg(test)]
impl CashedMemory {
    fn clean_cache(&mut self) {
        for set in &mut self.cache_sets {
            for line in &mut set.lines {
                line.valid = false;
            }
        }
    }

    // fn print_debug(&self, s: usize) {
    //     for line in &self.cache_sets[s].lines {
    //         println!("s: {}, valid: {}, tag: {}", s, line.valid, line.tag);
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cached_memory() {
        let mut memory = CashedMemory::new(1024);
        let addr = 11;
        let x = 0x1234567890ABCDEF;
        memory.write_quad(addr, x);
        assert_eq!(x, memory.read_quad(addr));

        let mut memory = CashedMemory::new(1024*1024);
        for i in 0..3 {
            memory.write(0x40100 + i, (0x40 + i).try_into().unwrap());
            memory.write(0x50100 + i, (0x40 + i).try_into().unwrap());
            memory.write(0x60100 + i, (0x40 + i).try_into().unwrap());
            memory.write(0x90200 + i, (0x50 + i).try_into().unwrap());
        }
        // memory.print_debug(1);
        memory.clean_cache();
        assert_eq!(memory.read(0x40100), 0x40);
        assert_eq!(memory.read(0x40101), 0x41);
        assert_eq!(memory.read(0x40102), 0x42);
        assert_eq!(memory.read(0x90200), 0x50);
        assert_eq!(memory.read(0x90201), 0x51);
        assert_eq!(memory.read(0x90202), 0x52);
        // memory.print_debug(1);
        // memory.print_debug(2);
        assert_eq!(4, memory.read_cache_hit);
        assert_eq!(2, memory.read_cache_miss);
    }
}

impl ByteArray {
    pub fn new(mem_size: usize) -> ByteArray {
        ByteArray {
            memory: vec![0; mem_size],
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

    // pub fn write_quad(&mut self, addr: usize, x: u64) {
    //     write_words(&mut self.memory, addr, x);
    // }

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
