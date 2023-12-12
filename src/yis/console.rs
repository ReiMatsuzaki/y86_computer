use super::ram::Ram;

pub struct Console {}

// Rule
// M[addr] is memory byte at address addr
// M[0x0100] = 0 => do nothing
//           = 1 => write bytes at console
// M[0x0110] = length of bytes
// M[0x0120] = bytes address

impl Console {
    pub fn new() -> Console {
        Console {}
    }

    pub fn cycle(&self, ram: &mut Ram) {
        let flag = ram.read(0x0100);
        if flag == 1 {
            self.write(ram);
            ram.write(0x0100, 0);
        }
    }

    fn write(&self, ram :&mut Ram) {
        let len = ram.read(0x0110);
        let addr = ram.read_quad(0x0120) as usize;
        for i in 0..len {
            let i = i as usize;
            let c = ram.read(addr + 8*i);
            print!("{}", c as char);
        }
    }
}