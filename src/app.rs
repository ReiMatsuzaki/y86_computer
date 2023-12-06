use std::{ffi::OsStr, path::Path};

use csapp::utils::print_bytes;

use crate::{
    yas, ycc,
    yis::{self, Y8R},
};

// Y86_64 simulator
pub fn run(filename: &str, command: &str, log_level: i64, wrange: Option<(usize, usize)>) -> u64 {
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
    bytes.resize(8000, 0x00);

    let bytes = match yas::write_bytes(statements, bytes) {
        Ok(a) => a,
        Err(e) => panic!("{}", e),
    };

    if log_level >= 1 {
        println!("\nbytesn");
        print_bytes(&bytes, Some(0), Some(1024));
    }

    if command == "build" {
        println!("\nbytes:");
        panic!("not implemented. command=build")
        // print_bytes(&bytes);
    } else if command == "run" {
        if log_level >= 0 {
            println!("yis start");
        }
        let mut machine = yis::make_machine(log_level, wrange);
        machine.load(0, &bytes);
        let maybe_cycle = machine.start();
        match maybe_cycle {
            Some(cycle) => {
                if log_level >= 0 {
                    println!("halted. cycle: {}", cycle)
                }
            }
            None => println!("too much cycles. stopped."),
        }
        if log_level >= 0 {
            println!("\nregisters:");
            machine.print_registers();
        }
        return machine.get_register(Y8R::RAX).try_into().unwrap();
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
        run(filename, command, log_level, wrange)
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
