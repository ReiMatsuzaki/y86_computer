/*  simple c compiler for Y86-64 */
mod token;
mod node;
mod tokenizer;
mod parser;
mod coder;

use crate::yas::Code;

pub const INIT_SP: u64 = 2816; // initial stack pointer

pub fn scompile(src: &str, verbose: i64) -> Vec<Code> {
    let tokens = tokenizer::tokenize(src);
    if verbose >= 1 {
        println!("tokens: {:?}", tokens)
    }

    let mut parser = parser::Parser::new(tokens);
    let prog = match parser.parse() {
        Ok(p) => p,
        Err(e) => {
            panic!("parse error\ntoken={0:?}\n\nmsg={1}", e.token, e.message)
        }
    };
    // FIXME:: print error message
    if verbose >= 1 {
        prog.display();
    }

    let mut coder = coder::Coder::new();
    let codes = coder.code(&prog);
    codes
}
