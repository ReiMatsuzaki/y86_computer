#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Op(char),
    Op2([char; 2]),
    Num(u64),
    Id(String),
    Int,
    Return,
    While,
    If,
}
