#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Op(char),
    Op2([char; 2]),
    Num(u64),
    Hex(u64),
    Str(String),
    Id(String),
    Int,
    Return,
    While,
    If,
}
