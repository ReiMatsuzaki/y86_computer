pub mod utils {
    pub fn print_bytes(xs: &Vec<u8>, start: Option<usize>, end: Option<usize>) {
        let start = start.unwrap_or(0);
        let end = end.unwrap_or(xs.len());
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
                    print!("{0:>02X} ", xs[16 * j + i]);
                }
            }
            println!("");
        }
    }
}

// Y86_64 simulator
