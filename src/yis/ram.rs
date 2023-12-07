// pub struct Ram {
//     pub mem: Vec<u8>,
// }

// impl Ram {
//   pub fn new(mem_size: usize) -> Ram {
//     Ram { mem: vec![0; mem_size] }
//   }
// }

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

