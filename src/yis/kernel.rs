use crate::yis::inst::Y8R;

use super::{inst::{Exception, Y8S}, cpu::Cpu};

pub struct Kernel {

}

impl Kernel {
    pub fn new() -> Kernel {
        Kernel {}
    }

    pub fn handle_exception(&self, e: Exception, cpu: &mut Cpu) -> Y8S {
        match e {
            Exception::DivideError => {
                println!("Kernel: divide by zero error found. Replace result with 1");
                cpu.set_register(Y8R::RAX, 1);
                Y8S::AOK
            },
            _ => {
                panic!("unknown exception")
            }
        }
    }
}