use std::{ffi::OsStr, path::Path};

use crate::{
    yas, ycc, yis::computer::Computer,
};

const INIT_MEM_POS: usize = 0x1000; // 0x1000 = 4096
const MEM_SIZE: usize = 1024 * 1024;

// Y86_64 simulator
pub fn run(filename: &str, command: &str, log_level: i64, wrange: Option<(usize, usize)>, num_proc: usize) -> u64 {
    let extension = Path::new(filename)
        .extension()
        .and_then(OsStr::to_str)
        .unwrap();
    let statements = match extension {
        "yc" => {
            if log_level >= 0 {
                println!("ycc start");
            }
            // let statements_old = ycc::compile(&contents);
            let statements = ycc::scompile(filename, log_level);
            statements
        }
        "ys" => {
            if log_level >= 0 {
                println!("yas parser start");
            }
            let statements = match yas::scan_file(&filename) {
                Result::Ok(ss) => ss,
                Result::Err(e) => panic!("{}", e),
            };
            statements
        }
        _ => panic!("unexpected extension"),
    };
    if log_level >= 1 {
        println!("\nyas statements:");
        for s in &statements {
            println!("{:?}", s);
        }
    }

    if log_level >= 0 {
        println!("yas coder start");
    }
    let mut bytes: Vec<u8> = Vec::new();
    bytes.resize(16*16*16*16, 0x00);
    let bytes = match yas::write_bytes(statements, bytes) {
        Ok(a) => a,
        Err(e) => panic!("{}", e),
    };

    // INIT_POS is meaning less.
    let mut computer = Computer::new(MEM_SIZE, INIT_MEM_POS, log_level, wrange);

    println!("load bytes to ram");
    for i in 0..num_proc {
        computer.set_ram_base_bound(i * 0x1_0000, 0x1_0000);
        computer.load(0, &bytes);
    }
    computer.set_ram_base_bound(0x0_0000, 0x1_0000);
    
    if log_level >= 1 {
        computer.print_ram();
    }

    if command == "build" {
        println!("\nbytes:");
        panic!("not implemented. command=build")
        // print_bytes(&bytes);
    } else if command == "run" {
        if log_level >= 0 {
            println!("yis start");
        }
        let maybe_res = computer.start();
        match maybe_res {
            Some((cycle, res)) => {
                if log_level >= 0 {
                    println!("halted. cycle: {}", cycle)
                }
                return res
            }
            None => panic!("too much cycles. stopped."),
        }
    } else {
        panic!("unexpected command")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_file(filename: &str) -> u64 {
        let command = "run";
        let log_level = -1;
        // let log_level = 2;
        let wrange = None;
        // let wrange = Some((0, 256));
        run(filename, command, log_level, wrange, 1)
    }

    #[test]
    fn test_plus() {
        let filename = "y86/tests/plus.yc";
        assert_eq!(0x6, run_file(filename));
    }

    #[test]
    fn test_arithmetric() {
        let filename = "y86/tests/arithmetric.yc";
        assert_eq!(0x21, run_file(filename));
    }

    #[test]
    fn test_var() {
        let filename = "y86/tests/4var.yc";
        assert_eq!(0xB, run_file(filename));
    }

    #[test]
    fn test_assign() {
        let filename = "y86/tests/5assign.yc";
        assert_eq!(0x4, run_file(filename));
    }

    #[test]
    fn test_while() {
        let filename = "y86/tests/6while.yc";
        assert_eq!(0x7, run_file(filename));
    }

    #[test]
    fn test_block() {
        let filename = "y86/tests/7block.yc";
        assert_eq!(0x18, run_file(filename));
    }

    #[test]
    fn test_def() {
        let filename = "y86/tests/8def.yc";
        assert_eq!(0x5, run_file(filename));
    }

    #[test]
    fn test_pointer() {
        let filename = "y86/tests/9pointer.yc";
        assert_eq!(0x5, run_file(filename));
    }

    #[test]
    fn test_array() {
        let filename = "y86/tests/10array.yc";
        assert_eq!(0x7, run_file(filename));
    }

    #[test]
    fn test_global() {
        let filename = "y86/tests/11global.yc";
        assert_eq!(0xE, run_file(filename));
    }

    #[test]
    fn test_string() {
        let filename = "y86/tests/12string.yc";
        assert_eq!(0x65, run_file(filename));
    }
}
