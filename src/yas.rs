pub(crate) mod code;
mod ys_scanner;
mod byte_writer;

use std::{fs, f32::consts::E};

use self::code::Code;

/*  ask chat-GPT
以下の構文をパースするrustの関数を書いてください。 ただしパースする関数の返り値はResult型を使ってください。
statement::=halt | nop | rrmovq R, R | irmovq V, R | rmmovq R, D(R) | mrmovq D(R), R | addq R, R | subq R, R | andq R, R | orq R, R | je D | jn D | call D | ret | pushq R | popq R
D ::= {integer} | Label
R ::= $RAX | $RCX | $RDX | $RBX | $RSP | $RBP | $RSI | $RDI | $R8 | $R9 | $R10 | $R11
V ::= \${integer} | Label
Label ::= [a-zA-Z]+
*/

pub fn parse_file(filename: &str)  -> Result<Vec<Code>, String> {
    let src = match fs::read_to_string(filename) {
        Err(e) => Err(format!("error at reading file: {}", e)),
        Ok(s) => Ok(s)
    }?;
    match ys_scanner::Scanner::scan(&src) {
        Err(e) => Err(format!("error at scanning: {}", e.message)),
        Ok(s) => Ok(s)
    }
    
}

pub fn write_bytes(codes: &Vec<Code>) -> Result<Vec<u8>, String> {
    byte_writer::code(codes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assemble() {
        let input = "halt";
        let expe = vec![0x00];
        let calc = ys_scanner::Scanner::new().scan_src(input).unwrap();
        let calc = write_bytes(&calc).unwrap();
        assert_eq!(expe, calc);
    }
}