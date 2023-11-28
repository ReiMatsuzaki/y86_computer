use std::{env, io, fs};

// use csapp_y86_64::make_machine;
use csapp::utils::print_bytes;
mod assembler;

fn main() -> io::Result<()>{
    // let ra: u8 = 0x0A;
    // let rb: u8 = 0x09;
    // let rc = (ra << 4) + rb;
    // println!("{0:x}, {1:x}, {2:x}", ra << 4, rb, rc);
    // return Ok(());

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::Other, "No file name provided"));
    }
    let filename = &args[1];
    let contents = fs::read_to_string(filename)?;
    match assembler::assemble(&contents) {
        Result::Ok(xs) => {
            print_bytes(&xs);
            // let machine = make_machine(0);
            // machine.load(0, &xs);
        },
        Result::Err(e) => {
            println!("{}", e);
        }
    }
    Ok(())
}
