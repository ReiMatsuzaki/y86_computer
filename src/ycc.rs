/*  simple c compiler for Y86-64 */

use crate::yas::{Statement, Value, Register};

#[derive(Debug, PartialEq)]
enum Token {
    Op(char),
    Num(u64),
    Id(String),
}

#[derive(PartialEq)]
enum Ast {
    Op(u64, Op, u64),
    // Num(i64),
    // Id(String),
    // Op(Op, Box<Ast>, Box<Ast>),
}

#[derive(PartialEq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

pub fn compile(src: &str) -> Vec<Statement> {
    let tokens = tokenize(src);
    let ast = parse(&tokens);
    code(&ast)
}

fn tokenize(src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = src.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            ' ' | '\t' | '\n' => continue,
            '+' | '-' | '*' | '/' | '(' | ')' => {
                tokens.push(Token::Op(c));
            },
            '0'..='9' => {
                let mut num = String::new();
                num.push(c);
                while let Some(&c) = chars.peek() {
                    match c {
                        '0'..='9' => {
                            num.push(c);
                            chars.next();
                        },
                        _ => break
                    }
                }
                tokens.push(Token::Num(num.parse::<u64>().unwrap()));
            },
            _ => {
                let mut id = String::new();
                id.push(c);
                while let Some(&c) = chars.peek() {
                    match c {
                        'a'..='z' | 'A'..='Z' | '0'..='9' => {
                            id.push(c);
                            chars.next();
                        },
                        _ => break
                    }
                }
                tokens.push(Token::Id(id));
            }
        }
    }
    tokens
}

fn parse(tokens: &Vec<Token>) -> Ast {
    match tokens.as_slice() {
        [Token::Num(a), Token::Op(o), Token::Num(b)] => Ast::Op(*a, parse_op(*o), *b),
        _ => panic!("unexpected token")
    }
}

fn parse_op(o: char) -> Op {
    match o {
        '+' => Op::Add,
        '-' => Op::Sub,
        '*' => Op::Mul,
        '/' => Op::Div,
        _ => panic!("unexpected operator")
    }
}

// fn parse(tokens: &Vec<Token>) -> Ast {
//     let mut ast = Ast::new();
//     let mut i = 0;
//     while i < tokens.len() {
//         match tokens[i] {
//             Token::Op(c) => {
//                 match c {
//                     '+' | '-' | '*' | '/' => {
//                         let op = match c {
//                             '+' => Op::Add,
//                             '-' => Op::Sub,
//                             '*' => Op::Mul,
//                             '/' => Op::Div,
//                             _ => panic!("unexpected operator")
//                         };
//                         let lhs = match tokens[i-1] {
//                             Token::Num(n) => Ast::Num(n),
//                             Token::Id(ref s) => Ast::Id(s.clone()),
//                             _ => panic!("unexpected token")
//                         };
//                         let rhs = match tokens[i+1] {
//                             Token::Num(n) => Ast::Num(n),
//                             Token::Id(ref s) => Ast::Id(s.clone()),
//                             _ => panic!("unexpected token")
//                         };
//                         ast.add_op(op, lhs, rhs);
//                         i += 2;
//                     },
//                     _ => panic!("unexpected operator")
//                 }
//             },
//             _ => panic!("unexpected token")
//         }
//     }
//     ast
// }

fn code(ast: &Ast) -> Vec<Statement> {
    match ast {
        Ast::Op(a, o, b) => {
            let mut stmts = Vec::new();
            stmts.push(Statement::Irmovq(Value::Integer(*a), Register::RAX));
            stmts.push(Statement::Irmovq(Value::Integer(*b), Register::RBX));
            match o {
                Op::Add => stmts.push(Statement::Addq(Register::RBX, Register::RAX)),
                Op::Sub => stmts.push(Statement::Subq(Register::RBX, Register::RAX)),
                _ => panic!("unexpected operator")
            }
            stmts
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let input = "1 + 2";
        let expe = vec![Token::Num(1), Token::Op('+'), Token::Num(2)];
        let calc = tokenize(input);
        assert_eq!(expe, calc);
    }
}