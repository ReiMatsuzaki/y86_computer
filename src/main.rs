use core::panic;
use std::{env, fs, io};

// use csapp_y86_64::make_machine;
use csapp::utils::print_bytes;
mod yas;
mod ycc;
use std::path::Path;
use std::ffi::OsStr;

fn main() -> io::Result<()> {
    // let ra: u8 = 0x0A;
    // let rb: u8 = 0x09;
    // let rc = (ra << 4) + rb;
    // println!("{0:x}, {1:x}, {2:x}", ra << 4, rb, rc);
    // return Ok(());

    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "usage: csapp <run|build> <filename>",
        ));
    }
    let command = &args[1];
    let filename = &args[2];
    let extension = Path::new(filename)
        .extension()
        .and_then(OsStr::to_str);

    println!("command: {}", command);
    println!("filename: {}", filename);

    let contents = fs::read_to_string(filename)?;
    let statements = match extension {
        Some("yc") => {
            println!("ycc start");
            let statements = ycc::compile(&contents);
            statements
        },
        Some("ys") => {
            println!("yas parser start");
            let statements = match yas::parse_body(&contents) {
                Result::Ok(ss) => ss,
                Result::Err(e) => panic!("{}", e),
            };
            statements
        },
        _ => panic!("unexpected extension"),
    };
    println!("\nyas statements:");
    for s in &statements {
        println!("{:?}", s);
    }

    let bytes = match yas::code(&statements) {
        Result::Ok(bs) => bs,
        Result::Err(e) => panic!("{}", e),
    };

    if command == "build" {
        println!("\nbytes:");
        print_bytes(&bytes);
    } else if command == "run" {
        panic!("not implemented");
    } else {
        panic!("unexpected command")
    }

    Ok(())
}