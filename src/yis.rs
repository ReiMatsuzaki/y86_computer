pub(crate) mod proc;
pub(crate) mod inst;
pub(crate) mod ram;

// assign 4kB(=4096 byte) for each process
// proc0: 0x0000 - 0x1FFF
// proc1: 0x1000 - 0x2FFF
// ...
// procF: 0xF000 - 0xFFFF
// store 16 processes at most
// 16 * 4kB = 64kB = 16 ** 4
// simulate 16 bit address space



#[cfg(test)]
mod tests {
}