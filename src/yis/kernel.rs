use crate::yis::inst::Y8R;

use super::{inst::{ProcError, Y8S}, proc::SeqProcessor};

pub struct Kernel {

}

impl Kernel {
    pub fn new() -> Kernel {
        Kernel {}
    }

    pub fn handle_exception(&self, e: ProcError, proccessor: &mut SeqProcessor) -> Y8S {
        match e {
            ProcError::DivideError => {
                println!("Kernel: divide by zero error found. Replace result with 1");
                proccessor.set_register(Y8R::RAX, 1);
                Y8S::AOK
            },
            _ => {
                panic!("unknown exception")
            }
        }
    }
}