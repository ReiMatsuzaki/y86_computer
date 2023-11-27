use csapp_y86_64::{encode_codefn, decode_codefn, split_byte, make_machine, Y8R, assembler};

extern crate csapp_y86_64;

#[test]
fn codefn_test() {
    let goods = vec![
        0x00,
        0x10,
        0x20,
        0x30,
        0x40,
        0x50,
        0x60,
        0x61,
        0x62,
        0x63,
        0x73,
        0x74,
        0x80,
        0x90,
        0xA0,
        0xB0,
    ];
    for x in goods {
        match decode_codefn(x).map(encode_codefn) {
            None => panic!("failed to decode. {}", x),
            Some(y) => assert_eq!(x, y)
        }
    }
    let bads = vec![0x01, 0x12, 0x75];
    for x in bads {
        assert!(decode_codefn(x).is_none())
    }
}
#[test]
fn split_byte_test() {
    let x: u8 = 0xAB;
    let (y, z) = split_byte(x);
    assert_eq!(0x0A, y);
    assert_eq!(0x0B, z);
}
#[test]
fn seq_processor_test() {
    let mut machine = make_machine(3);
    let insts: [u8; 4*10 + 2*9 + 3*2 + 2*1] = [
        // 0x00: IRMOVQ $9  $rdx
        0x30, 0xF2, 0x09, 0, 0, 0, 0, 0, 0, 0,
        // 0x0A: IRMOVQ $21 $rbx
        0x30, 0xF3, 0x15, 0, 0, 0, 0, 0, 0, 0, 
        // 0x14: subq rdx rbx
        0x61, 0x23, 
        // -> rdx=0x09, rbx=0x0C
        // 0x16: IRMOVQ $128 $rsp
        0x30, 0xF4, 0x80, 0, 0, 0, 0, 0, 0, 0, 
        // -> rsp=0x80
        // 0x20: RMMOVQ $rsp 100(%rbx)
        0x40, 0x43, 0x64, 0, 0, 0, 0, 0, 0, 0, 
        // -> M[0x70]=0x80    // 0x64+0x0C=0x70
        // 0x02A: PUSHQ $rdx
        0xA0, 0x2F, 
        // -> M[0x78]=0x09    // 0x80-0x08=0x78
        // 0x2C: POPQ $rax
        0xB0, 0x0F, 
        // -> rax=0x09
        // 0x2E: je done
        0x73, 0x40, 0, 0, 0, 0, 0, 0, 0, 
        // 0x37: call proc
        0x80, 0x41, 0, 0, 0, 0, 0, 0, 0, 
        // 0x40: halt
        0x00, 
        // 0x41: ret
        0x90,
    ];
    machine.load(0, &insts);
    machine.start();
    assert_eq!(0x80, machine.get_memory(0x70 as usize));
    assert_eq!(0x40, machine.get_memory(0x78 as usize));
    assert_eq!(0x09, machine.get_register(Y8R::RAX));
}

// #[test]
// fn assembler_test() {
//     let a = assembler::make_assembler();
//     let line = String::from("irmovq $9, %rdx");
//     let xs = assembler.assemble_line(&line);
//     assert_eq!(0x30, xs[0]);
//     assert_eq!(0xF2, xs[1]);
//     assert_eq!(0x09, xs[2]);
//     assert_eq!(0x00, xs[3]);
// }