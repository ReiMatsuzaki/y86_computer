/*  simple c compiler for Y86-64 */
mod token;
mod node;
mod tokenizer;
mod parser;
mod coder;

use std::{process, fs};

use crate::yas::Code;

pub const INIT_SP: u64 = 2816; // initial stack pointer

pub fn scompile(filename: &str, verbose: i64) -> Vec<Code> {
    let src = match fs::read_to_string(filename) {
        Err(e) => {
            eprintln!("error at reading file: {}", e);
            process::exit(1)
        }
        Ok(s) => s
    };
    let src = if !src.contains("main") {
        String::from("int main() {\n int a;\n int b;\n int c;\n") + &src + "\n}"
    } else {
        src
    };

    let (tokens, token_info) = tokenizer::tokenize(&src);
    if verbose >= 1 {
        println!("tokens: {:?}", tokens)
    }

    let mut parser = parser::Parser::new(tokens);
    let prog = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            let ti = &token_info[e.pos];
            let line = src.lines().nth(ti.line_num - 1).unwrap();
            eprintln!("error at parsing");
            eprintln!("{}:{}: ", filename, ti.line_num);
            eprintln!("{}", line);
            let pos = ti.col_num;
            eprint!("{: >1$}", "^", pos);
            eprintln!("{}", e.message);

            process::exit(1)
        }
    };

    if verbose >= 1 {
        prog.display();
    }

    let mut coder = coder::Coder::new();
    let codes = coder.code(&prog);
    codes
}
