use crate::instructions::*;
use std::collections::HashMap;
use std::rc::Rc;

type Ident = Rc<String>;

pub enum Val<T> {
    Ident(Ident),
    Literal(T),
}

pub enum IRIns {
    InitStore32 {
        dest: Ident,
        val: i32,
    },
    Store32 {
        dest: Ident,
        val: i32,
    },
    Add32 {
        dest: Ident,
        val1: Val<i32>,
        val2: Val<i32>,
    },
    Sub32 {
        dest: Ident,
        val1: Val<i32>,
        val2: Val<i32>,
    },
    Mul32 {
        dest: Ident,
        val1: Val<i32>,
        val2: Val<i32>,
    },
    Div32 {
        dest: Ident,
        val1: Val<i32>,
        val2: Val<i32>,
    },
    Print32 {
        val: Val<i32>,
    },
}

#[derive(Copy, Clone)]
pub struct StackLoc {
    rbp_offset: u32,
    size: u32,
}

struct StackMap {
    map: HashMap<Ident, StackLoc>,
    rsp: u32,
}

impl StackMap {
    fn new() -> Self {
        Self {
            map: HashMap::default(),
            rsp: 0,
        }
    }

    fn get(&self, ident: &Ident) -> StackLoc {
        self.map.get(ident).unwrap().clone()
    }

    fn insert(&mut self, ident: Ident, size: u32) -> u32 {
        let rbp_offset = self.rsp + size;
        self.map.insert(ident, StackLoc { rbp_offset, size });

        self.rsp += size;

        self.rsp
    }
}
use crate::encode::InsPtr;
use crate::instructions::GPReg::*;
use codegen::pretty_code_vec;

pub fn ir_encode_fn(inss: Vec<IRIns>) -> Vec<InsPtr> {
    let mut encoded = vec![];
    let mut stack = StackMap::new();

    encoded.extend(pretty_code_vec![
        // PushR Rsp;
        PushR Rcx;
        PushR Rbx;
        PushR Rdx;
        PushR Rsi;
        PushR Rdi;

        PushR Rbp;
        Mov64RR Rbp Rsp;


        // Mov64RR Rbp Rsp;
        Sub64RImm Rsp 0xff; //this space is used by all the variables, so make sure it is
    //adequate
        Mov64RR Rbx Rdi;
    ]);

    for ins in inss {
        use IRIns::*;
        match ins {
            InitStore32 { dest, val } => {
                stack.insert(Rc::clone(&dest), 8);
                let StackLoc { rbp_offset, .. } = stack.get(&dest);
                let off = 0 - rbp_offset as i32;

                encoded.extend(pretty_code_vec![
                    Mov64Md32Imm32 Rbp off val;
                ]);
            }
            Add32 { dest, val1, val2 } => {
                let StackLoc { rbp_offset, .. } = stack.get(&dest);
                let off = 0 - rbp_offset as i32;

                match (val1, val2) {
                    (Val::Ident(v1), Val::Literal(l2)) => {
                        let StackLoc { rbp_offset: v1_off, .. } = stack.get(&dest);
                        let off_1 = 0 - v1_off as i32;
                        encoded.extend(pretty_code_vec![
                            Mov64RMd32 Rdi Rbp off_1;
                            Mov64RImm64 Rsi (l2.into());
                            AddRR Rdi Rsi;
                            Mov64Md32R Rbp off_1 Rdi;
                        ]);
                    }
                    _ => unimplemented!(),
                }
            }
            Print32 { val } => match val {
                Val::Literal(v) => {
                    let v = v.into();
                    encoded.extend(pretty_code_vec![
                        Mov64RImm64 Rdi v;
                        CallM Rbx;
                    ]);
                }
                Val::Ident(dest) => {
                    let StackLoc { rbp_offset, .. } = stack.get(&dest);

                    let off = 0 - rbp_offset as i32;
                    let rsp = 16 as i32;
                    encoded.extend(pretty_code_vec![
                        Mov64RMd32 Rdi Rbp off;
                            // Mov64RR Rdi Rdx;
                        Mov64RR Rax Rbx;
                        // PushR Rdi;

                        // Mov64RR Rdi Rbp;
                    //  Pus
                        // PushR Rax;
                        // Sub64RImm Rsp 0x28;
                        // Mov64RR Rbx Rbx;
                    // CallR Rax;
                        CallM Rax;
                        // Add64RImm Rsp 0x28;
                        // PopR Rax;

                        // PopR Rdi;

                        // Mov64RR Rdi Rbp;
                        // CallM Rsi;
                    ]);
                }
            },
            _ => todo!(),
        }
    }

    encoded.extend(pretty_code_vec![
        // MovRR Rax Rb;
        // Mov64RMd32 Rax Rbp -4;
       Add64RImm Rsp 0xff;
        // MovRR Rax Rcx;
       // PopR Rsp;
        // PopR Rbp;
        Mov64RR Rsp Rbp;
        PopR Rbp;

        PopR Rdi;
        PopR Rsi;
        PopR Rdx;
        PopR Rcx;
        PopR Rbx;
        // PopR Rcx;
       // Leave;
        // Add64RImm Rsp 0x10;
        Ret;
    ]);

    encoded
}
//
// extern "C" fn f(i: i32) {
//
//         println!("value = {i}");
// }

#[test]
fn test_ir_encode() {
    let mut f = |i: i32| {
        println!("value = {i}");
    };

    let fn_mut_ref = &mut f;
    let fc_ = libffi::high::Closure1::new(&mut f);
    let fc = fc_.code_ptr();
    let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

    let inss = ir_encode_fn(vec![
        IRIns::InitStore32 {
            dest: Rc::new("hello".to_string()),
            val: 123,
        },
        IRIns::Print32 {
            val: Val::Literal(12),
        },
        IRIns::Print32 {
            val: Val::Ident(Rc::new("hello".to_string())),
        },
    ]);

    println!(
        "result = {}",
        crate::quick_run::<extern "C" fn(i32) -> (), i32>(fc_ptr, inss)
    );
}
