pub(crate) mod computer;
pub(crate) mod proc;
pub(crate) mod inst;
pub(crate) mod ram;
pub(crate) mod console;

// assign 4kB(=4096 byte) for each process
// proc0: 0x0000 - 0x1FFF
// proc1: 0x1000 - 0x2FFF
// ...
// procE: 0xE000 - 0xEFFF // used for kernel
//        0xE200 - 0xE2FF // exception table
//        0xE300 - 0xEFFF // kernel main code
// procF: 0xF000 - 0xFFFF // used for device
// store 16 processes at most
// 16 * 4kB = 64kB = 16 ** 4
// simulate 16 bit address space
