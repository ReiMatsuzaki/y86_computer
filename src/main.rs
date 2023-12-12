use std::io;

// use csapp_y86_64::make_machine;
mod app;
mod yis;
mod yas;
mod ycc;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(help="execution type. (build|run)")]
    command: String,

    #[arg(help="input file name")]
    filename: String,
    
    #[arg(short, long, default_value_t = 0)]
    log_level: i64,

    #[arg(short, long, default_value_t = 1)]
    num_proc: usize,

    #[arg(short, long)]
    watch_memory_range: Option<String>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();
    let command = &args.command;
    let filename = &args.filename;
    let log_level = &args.log_level;
    let num_proc = &args.num_proc;
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
    println!("waching memory range: {:?}", wrange);
    let res = app::run(&filename, command, *log_level, wrange, *num_proc);
    println!("result: {}", res);
    Ok(())
}
