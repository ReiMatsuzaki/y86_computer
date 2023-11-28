use std::{env, fs, io};

// use csapp_y86_64::make_machine;
use csapp::utils::print_bytes;
mod yas;
mod ycc;

fn main() -> io::Result<()> {
    // let ra: u8 = 0x0A;
    // let rb: u8 = 0x09;
    // let rc = (ra << 4) + rb;
    // println!("{0:x}, {1:x}, {2:x}", ra << 4, rb, rc);
    // return Ok(());

    let args: Vec<String> = env::args().collect();
    if args.len() != 4 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "No file name provided",
        ));
    }
    let command = &args[1];
    let filename = &args[2];

    let contents = fs::read_to_string(filename)?;
    if command == "yas" {
        match yas::assemble(&contents) {
            Result::Ok(xs) => {
                print_bytes(&xs);
                // let machine = make_machine(0);
                // machine.load(0, &xs);
            }
            Result::Err(e) => {
                println!("{}", e);
            }
        }
    } else if command == "ycc" {
        let statements = ycc::compile(&contents);
        let symbol_table = yas::build_symbol_table(&statements);
        match yas::assemble_many(&statements, &symbol_table) {
            Result::Ok(xs) => {
                print_bytes(&xs);
            }
            Result::Err(e) => {
                println!("{}", e);
            }
        }
    } else {
        panic!("unexpected command")
    }

    Ok(())
}
