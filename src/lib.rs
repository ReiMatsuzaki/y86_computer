pub mod utils {
    pub fn print_bytes(xs: &Vec<u8>) {
        let maxj = 1 + xs.len() / 16;
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
        for j in 0..maxj {
            print!("{0:<2X}: ", j);
            for i in 0..16 {
                let addr = 16 * j + i;
                if addr < xs.len() {
                    print!("{0:>02X} ", xs[16 * j + i]);
                }
            }
            println!("");
        }
    }
}

// Y86_64 simulator
