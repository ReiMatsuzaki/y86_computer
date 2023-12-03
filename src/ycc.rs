/*  simple c compiler for Y86-64 */
mod token;
mod node;
mod tokenizer;
mod parser;
mod coder;

use crate::yas::Statement;
// use crate::ycc::tokenizer;
// use crate::ycc::parser;

pub const INIT_SP: u64 = 2816; // initial stack pointer

pub fn scompile(src: &str, verbose: i64) -> Vec<Statement> {
    let tokens = tokenizer::tokenize(src);
    if verbose >= 1 {
        println!("tokens: {:?}", tokens)
    }
    let mut parser = parser::Parser::new(tokens);
    let prog = parser.parse();
    if verbose >= 1 {
        prog.display();
    }
    let coder = coder::Coder {};
    let codes = coder.code(&prog);
    codes
}
