use std::{env, io, fs};

// use csapp_y86_64::make_machine;
mod assembler;

fn main() -> io::Result<()>{
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::Other, "No file name provided"));
    }
    let filename = &args[1];
    let contents = fs::read_to_string(filename)?;
    match assembler::assemble(&contents) {
        Result::Ok(xs) => {
            for x in xs {
                print!("{:02X} ", x);
            }
            // let machine = make_machine(0);
            // machine.load(0, &xs);
        },
        Result::Err(e) => {
            println!("{}", e);
        }
    }
    Ok(())
}
