use csapp_y86_64::make_machine;



fn test2() {
    let mut machine = make_machine(3);
    let insts: [u8; 4*10 + 2*9 + 3*2 + 2*1] = [
        0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $9  $rdx
        0x30, 0xF3, 0x15, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $21 $rbx
        0x61, 0x23, // subq rdx rbx
        // 0x61, 0x33, // subq rbx rbx
        0x30, 0xF4, 0x80, 0, 0, 0, 0, 0, 0, 0, // IRMOVQ $128 $rsp
        0x40, 0x43, 0x64, 0, 0, 0, 0, 0, 0, 0, // RMMOVQ $rsp 100(%rbx)
        0xA0, 0x2F, // PUSHQ $rdx
        0xB0, 0x0F, // POPQ $rax
        0x73, 0x40, 0, 0, 0, 0, 0, 0, 0, // je done
        0x80, 0x41, 0, 0, 0, 0, 0, 0, 0, // call proc
        0x00, // halt
        0x90, // ret
    ];
    machine.load(0, &insts);
    machine.start();
}
fn main() {
    test2();
}
