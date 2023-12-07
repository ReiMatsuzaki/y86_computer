use super::ram::Ram;

pub struct Console {}

// Rule
// M[addr] is memory byte at address addr
// M[0xE000] = 0 => do nothing
//           = 1 => write bytes at console
// M[0xE001] = length of bytes
// M[0xE100 .. 0xE1FF] is buffer

impl Console {
    pub fn new() -> Console {
        Console {}
    }

    pub fn cycle(&self, ram: &mut Ram) {
        let flag = ram.read(0xE000);
        if flag == 1 {
            self.write(ram);
            ram.write(0xE000, 0);
        }
    }

    fn write(&self, ram :&Ram) {
        let len = ram.read(0xE001);
        for i in 0..len {
            let c = ram.read(0xE100 + i as usize);
            print!("{}", c as char);
        }
    }
}