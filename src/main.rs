use core::panic;
use std::{fs, io};

// use csapp_y86_64::make_machine;
use csapp::utils::print_bytes;
mod yas;
mod ycc;
mod yis;
// extern crate clap;
// use clap::{Arg, App};
use std::ffi::OsStr;
use std::path::Path;


use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(help="execution type. (build|run)")]
    command: String,

    #[arg(help="input file name")]
    filename: String,
    
    #[arg(short, long, default_value_t = 0)]
    log_level: i64,

    #[arg(short, long)]
    watch_memory_range: Option<String>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();
    let command = &args.command;
    let filename = &args.filename;
    let log_level = &args.log_level;
    let watch_memory_range = &args.watch_memory_range.as_ref();
    println!("command: {}", command);
    println!("filename: {}", filename);
    println!("log_level: {}", log_level);

    let wrange = watch_memory_range.and_then(|s| {
        let x: Vec<&str> = s.split(":").collect();
        match x.as_slice() {
            [start, end] => Some((start.parse::<usize>().unwrap(), end.parse::<usize>().unwrap())),
            _ => None,
        }
    });
    println!("{:?}", wrange);

    let extension = Path::new(filename).extension().and_then(OsStr::to_str);
    let contents = fs::read_to_string(filename)?;
    let statements = match extension {
        Some("yc") => {
            println!("ycc start");
            let statements = ycc::compile(&contents);
            statements
        }
        Some("ys") => {
            println!("yas parser start");
            let statements = match yas::parse_body(&contents) {
                Result::Ok(ss) => ss,
                Result::Err(e) => panic!("{}", e),
            };
            statements
        }
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
        let mut machine = yis::make_machine(*log_level, wrange);
        println!("\nload bytes into memory");
        machine.load(0, &bytes);

        println!("machine start");
        machine.start();

        println!("machine halt.");
        println!("\nregisters:");
        machine.print_registers();
    } else {
        panic!("unexpected command")
    }

    Ok(())
}
