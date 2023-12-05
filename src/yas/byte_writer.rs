use std::collections::HashMap;

use super::code::{Code, Register, Imm, Dest};

fn byte_length(statement: &Code) -> u64 {
    match statement {
        Code::Label(_) => 0,
        Code::Halt => 1,
        Code::Nop => 1,

        Code::Rrmovq(_, _) => 2,
        Code::Irmovq(_, _) => 10,
        Code::Rmmovq(_, _) => 10,
        Code::Mrmovq(_, _) => 10,

        Code::Addq(_, _) => 2,
        Code::Subq(_, _) => 2,
        Code::Andq(_, _) => 2,
        Code::Orq(_, _) => 2,
        Code::Mulq(_, _) => 2,
        Code::Divq(_, _) => 2,        

        Code::Jmp(_) => 9,
        // Statement::Jle(_) => 9,
        // Statement::Jl(_) => 9,
        Code::Je(_) => 9,
        Code::Jne(_) => 9,

        Code::Cmovl(_, _) => 2,
        Code::Cmove(_, _) => 2,
        Code::Cmovne(_, _) => 2,

        Code::Call(_) => 9,
        Code::Ret => 1,
        Code::Pushq(_) => 2,
        Code::Popq(_) => 2,
    }
}

pub fn build_symbol_table(statements: &Vec<Code>) -> HashMap<String, u64> {
    let mut table = HashMap::new();
    let mut addr = 0;
    for statement in statements {
        if let Code::Label(s) = statement {
            table.insert(s.to_string(), addr);
        }
        addr += byte_length(&statement);
    }
    table
}

fn ass_reg(ra: Option<&Register>, rb: Option<&Register>) -> u8 {
    fn f(r: Option<&Register>) -> u8 {
        match r {
            None => 0x0F,
            Some(r) => match r {
                Register::RAX => 0x00,
                Register::RCX => 0x01,
                Register::RDX => 0x02,
                Register::RBX => 0x03,
                Register::RSP => 0x04,
                Register::RBP => 0x05,
                Register::RSI => 0x06,
                Register::RDI => 0x07,
                Register::R8 => 0x08,
                Register::R9 => 0x09,
                Register::R10 => 0x0A,
                Register::R11 => 0x0B,
            },
        }
    }
    (f(ra) << 4) + f(rb)
}

fn ass_imm(v: &Imm, symbol_table: &HashMap<String, u64>) -> Result<[u8; 8], String> {
    let x: u64 = match v {
        Imm::Integer(i) => *i,
        Imm::Label(s) => symbol_table
            .get(s)
            .ok_or(format!("failed to find symbol: {}", s))?
            .clone(),
    };
    let mut xs = x.to_be_bytes();
    xs.reverse();
    Result::Ok(xs)
}

fn ass_dest(d: &Dest, symbol_table: &HashMap<String, u64>) -> Result<[u8; 8], String> {
    // FIXME: duplicated code
    let x: u64 = match d {
        Dest::Integer(i) => *i,
        Dest::Label(s) => symbol_table
            .get(s)
            .ok_or(format!("failed to find symbol: {}", s))?
            .clone(),
    };
    let mut xs = x.to_be_bytes();
    xs.reverse();
    Result::Ok(xs)
}

fn assemble_one(
    statement: &Code,
    symbol_table: &HashMap<String, u64>,
) -> Result<Vec<u8>, String> {
    fn f_v(head: u8, d: &Dest, symbol_table: &HashMap<String, u64>) -> Result<Vec<u8>, String> {
        let xs = ass_dest(d, symbol_table)?;
        Result::Ok(vec![
            head, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
        ])
    }
    match statement {
        Code::Label(_) => Result::Ok(Vec::new()),
        Code::Halt => Result::Ok(vec![0x00]),
        Code::Nop => Result::Ok(vec![0x10]),

        Code::Rrmovq(ra, rb) => Result::Ok(vec![0x20, ass_reg(Some(ra), Some(rb))]),
        Code::Irmovq(v, rb) => {
            let r = ass_reg(None, Some(rb));
            let xs = ass_imm(v, symbol_table)?;
            Result::Ok(vec![
                0x30, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }
        Code::Rmmovq(ra, a) => {
            let r = ass_reg(Some(ra), Some(&a.register));
            let xs = ass_dest(&a.dest, symbol_table)?;
            Result::Ok(vec![
                0x40, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }
        Code::Mrmovq(a, ra) => {
            let r = ass_reg(Some(ra), Some(&a.register));
            let xs = ass_dest(&a.dest, symbol_table)?;
            Result::Ok(vec![
                0x50, r, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7],
            ])
        }

        Code::Addq(ra, rb) => Result::Ok(vec![0x60, ass_reg(Some(ra), Some(rb))]),
        Code::Subq(ra, rb) => Result::Ok(vec![0x61, ass_reg(Some(ra), Some(rb))]),
        Code::Andq(ra, rb) => Result::Ok(vec![0x62, ass_reg(Some(ra), Some(rb))]),
        Code::Orq(ra, rb) => Result::Ok(vec![0x63, ass_reg(Some(ra), Some(rb))]),
        Code::Mulq(ra, rb) => Result::Ok(vec![0x64, ass_reg(Some(ra), Some(rb))]),
        Code::Divq(ra, rb) => Result::Ok(vec![0x65, ass_reg(Some(ra), Some(rb))]),

        Code::Jmp(d) => f_v(0x70, d, symbol_table),
        // Statement::Jle(d) => f_v(0x71, d, symbol_table),        
        // Statement::Jl(d) => f_v(0x72, d, symbol_table),
        Code::Je(d) => f_v(0x73, d, symbol_table),
        Code::Jne(d) => f_v(0x74, d, symbol_table),

        Code::Cmovl(ra, rb) => Result::Ok(vec![0x22, ass_reg(Some(ra), Some(rb))]),
        Code::Cmove(ra, rb) => Result::Ok(vec![0x23, ass_reg(Some(ra), Some(rb))]),
        Code::Cmovne(ra, rb) => Result::Ok(vec![0x24, ass_reg(Some(ra), Some(rb))]),        

        Code::Call(d) => f_v(0x80, d, symbol_table),
        Code::Ret => Result::Ok(vec![0x90]),
        Code::Pushq(ra) => Result::Ok(vec![0xA0, ass_reg(Some(ra), None)]),
        Code::Popq(ra) => Result::Ok(vec![0xB0, ass_reg(Some(ra), None)]),
    }
}

pub fn assemble_many(
    statements: &Vec<Code>,
    symbol_table: &HashMap<String, u64>,
) -> Result<Vec<u8>, String> {
    let mut xs = Vec::new();
    for s in statements {
        let mut ys = assemble_one(s, symbol_table)?;
        xs.append(&mut ys);
    }
    Result::Ok(xs)
}

pub fn code(statements: &Vec<Code>) -> Result<Vec<u8>, String> {
    let symbol_table = build_symbol_table(&statements);
    assemble_many(&statements, &symbol_table)
}

// fn assemble(body: &str) -> Result<Vec<u8>, String> {
//     let statements = parse_body(body)?;
//     code(&statements)
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ass_imm() {
        let v = Imm::Integer(9);
        let st = HashMap::new();
        let calc = ass_imm(&v, &st);
        let expe: [u8; 8] = [9, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(Result::Ok(expe), calc);

        let v = Imm::Label(String::from("apple"));
        let mut st = HashMap::new();
        st.insert(String::from("apple"), 13);
        let calc = ass_imm(&v, &st);
        let expe: [u8; 8] = [13, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(Result::Ok(expe), calc);
    }

    #[test]
    fn test_symbol_table() {
        let statements: Vec<Code> = vec![
            Code::Halt,
            Code::Nop,
            Code::Label("orange".to_string()),
            Code::Rrmovq(Register::RAX, Register::RCX),
            Code::Label("apple".to_string()),
            Code::Irmovq(Imm::Integer(100), Register::RAX),
            Code::Label("peach".to_string()),
        ];
        let tab = build_symbol_table(&statements);
        assert_eq!(tab.get("halt"), None);
        assert_eq!(tab.get("orange"), Some(2).as_ref());
        assert_eq!(tab.get("apple"), Some(4).as_ref());
        assert_eq!(tab.get("peach"), Some(14).as_ref());
    }
}
