use super::code::Code;


fn parse_statement(input: &str) -> Result<Code, String> {
    let input = match input.find("//") {
        Some(i) => input[..i].trim(),
        None => input.trim(),
    };
    match input.find(":") {
        Some(i) => Ok(Code::Label(input[..i].to_string())),
        None => parse_instrument(input),
    }
}

fn split_instrument(input: &str) -> Vec<&str> {
    let input = input.trim_start();
    match input.find(" ") {
        None => vec![input],
        Some(i) => {
            let head = &input[..i];
            let tail = &input[i..];
            let args = tail.split(",");
        
            let mut parts = vec![head];
            for a in args {
                parts.push(a.trim());
            }
            parts
        }
    }
}

fn parse_instrument(input: &str) -> Result<Code, String> {
    let parts: Vec<&str> = split_instrument(input);
    match parts.as_slice() {
        ["halt"] => Ok(Code::Halt),
        ["nop"] => Ok(Code::Nop),
        ["rrmovq", ra, rb] => Ok(Code::Rrmovq(ra.parse()?, rb.parse()?)),
        ["irmovq", v, rb] => Ok(Code::Irmovq(v.parse()?, rb.parse()?)),
        ["rmmovq", ra, m] => Ok(Code::Rmmovq(ra.parse()?, m.parse()?)),
        ["mrmovq", m, ra] => Ok(Code::Mrmovq(m.parse()?, ra.parse()?)),
        ["addq", ra, rb] => Ok(Code::Addq(ra.parse()?, rb.parse()?)),
        ["subq", ra, rb] => Ok(Code::Subq(ra.parse()?, rb.parse()?)),
        ["andq", ra, rb] => Ok(Code::Andq(ra.parse()?, rb.parse()?)),
        ["orq", ra, rb] => Ok(Code::Orq(ra.parse()?, rb.parse()?)),
        ["mulq", ra, rb] => Ok(Code::Mulq(ra.parse()?, rb.parse()?)),
        ["divq", ra, rb] => Ok(Code::Divq(ra.parse()?, rb.parse()?)),
        ["je", m] => Ok(Code::Je(m.parse()?)),
        ["jne", m] => Ok(Code::Jne(m.parse()?)),
        ["cmove", ra, rb] => Ok(Code::Cmove(ra.parse()?, rb.parse()?)),
        ["cmovne", ra, rb] => Ok(Code::Cmovne(ra.parse()?, rb.parse()?)),
        ["call", m] => Ok(Code::Call(m.parse()?)),
        ["ret"] => Ok(Code::Ret),
        ["pushq", ra] => Ok(Code::Pushq(ra.parse()?)),
        ["popq", ra] => Ok(Code::Popq(ra.parse()?)),
        _ => Err(format!("statement parts mismatched: {0:?}", parts.as_slice())),
    }
}

pub fn parse_body(body: &str) -> Result<Vec<Code>, String> {
    let mut statements = Vec::new();
    for line in body.lines() {
        if !line.is_empty() {
            statements.push(parse_statement(line)?);
        }
    }
    Ok(statements)
}

#[cfg(test)]
mod tests {
    use crate::yas::code::{Register, Imm, Dest, ModDest};

    use super::*;

    #[test]
    fn test_parse_body() {
        assert_eq!(
            parse_body("halt\nnop\nrrmovq %rax, %rcx\nirmovq $100, %rax\n"),
            Ok(vec![
                Code::Halt,
                Code::Nop,
                Code::Rrmovq(Register::RAX, Register::RCX),
                Code::Irmovq(Imm::Integer(100), Register::RAX),
            ]),
        );
    }

    #[test]
    fn test_parse_halt() {
        assert_eq!(parse_statement("halt   //comment"), Ok(Code::Halt));
    }

    #[test]
    fn test_parse_nop() {
        assert_eq!(parse_statement("nop"), Ok(Code::Nop));
    }

    #[test]
    fn test_parse_rrmovq() {
        assert_eq!(
            parse_statement("  rrmovq %rax , %rcx"),
            Ok(Code::Rrmovq(Register::RAX, Register::RCX))
        );
    }

    #[test]
    fn test_parse_irmovq() {
        assert_eq!(
            parse_statement("irmovq $100, %rax"),
            Ok(Code::Irmovq(Imm::Integer(100), Register::RAX))
        );

        assert_eq!(
            parse_statement("mrmovq  10(%rbx), %rax "),
            Ok(Code::Mrmovq(
                ModDest {
                    dest: Dest::Integer(10),
                    register: Register::RBX
                },
                Register::RAX,
            ))
        );

        assert_eq!(
            parse_statement("rmmovq    %rax , 12(%rbx)  "),
            Ok(Code::Rmmovq(
                Register::RAX,
                ModDest {
                    dest: Dest::Integer(12),
                    register: Register::RBX
                },
            ))
        );

        assert_eq!(
            parse_statement("cmove    %rax , %rbx  "),
            Ok(Code::Cmove(
                Register::RAX,
                Register::RBX))
        );

        assert_eq!(
            parse_statement("cmovne    %rax , %rbx  "),
            Ok(Code::Cmovne(
                Register::RAX,
                Register::RBX))
        );
    }

    #[test]
    fn test_parse_opq() {
        assert_eq!(
            parse_statement("  addq    %rsi,  %rdi  "),
            Ok(Code::Addq(Register::RSI, Register::RDI)),
        );

        assert_eq!(
            parse_statement("subq    %rbp , %r8  "),
            Ok(Code::Subq(Register::RBP, Register::R8)),
        );
    
        assert_eq!(
            parse_statement("andq    %r9,  %r10  "),
            Ok(Code::Andq(Register::R9, Register::R10)),
        );

        assert_eq!(
            parse_statement("orq    %rax  ,%r11 "),
            Ok(Code::Orq(Register::RAX, Register::R11)),
        );

        assert_eq!(
            parse_statement("  mulq  %r9,  %r10  "),
            Ok(Code::Mulq(Register::R9, Register::R10)),
        );

        assert_eq!(
            parse_statement(" divq  %rax  ,%r11 "),
            Ok(Code::Divq(Register::RAX, Register::R11)),
        );
    }

    #[test]
    fn test_parse_jxx() {
        assert_eq!(
            parse_statement("je    100  "),
            Ok(Code::Je(Dest::Integer(100))),
        );
        assert_eq!(
            parse_statement("je    done  "),
            Ok(Code::Je(Dest::Label(String::from("done")))),
        );

        assert_eq!(
            parse_statement("jne    100  "),
            Ok(Code::Jne(Dest::Integer(100))),
        );
    }

    #[test]
    fn test_parse_call() {
        assert_eq!(
            parse_statement("call    100  "),
            Ok(Code::Call(Dest::Integer(100))),
        );
    }

    #[test]
    fn test_parse_ret() {
        assert_eq!(parse_statement("ret"), Ok(Code::Ret),);
    }

    #[test]
    fn test_parse_pushq() {
        assert_eq!(
            parse_statement("pushq    %rax  "),
            Ok(Code::Pushq(Register::RAX)),
        );
    }

    #[test]
    fn test_parse_popq() {
        assert_eq!(
            parse_statement("popq    %rax  "),
            Ok(Code::Popq(Register::RAX)),
        );
    }       
}