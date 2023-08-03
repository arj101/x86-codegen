#![allow(dead_code, unused_variables, path_statements, unused_must_use)]

use codegen::x86;
use libffi::high::{Closure0, ClosureMut0, ClosureMut1};
use std::{mem::MaybeUninit, process::Stdio};

unsafe fn code_alloc_inner(code: &[u8]) -> *const () {
    let mut code_mem_uninit: MaybeUninit<&mut [u8]> = MaybeUninit::uninit();
    let code_mem_ptr = code_mem_uninit.as_mut_ptr();

    let alloc_size = code.len().max(4096);

    const PAGE_SIZE: usize = 4096;
    //allocate pagesize aligned memory
    let alloc_result = libc::posix_memalign(code_mem_ptr as *mut *mut _, PAGE_SIZE, alloc_size);

    if alloc_result != 0 {
        panic!(
            "code_alloc_inner: memory allocation failed with {} (tried to allocate {alloc_size} bytes)",
            if alloc_result == libc::ENOMEM {
                "ENOMEM".to_owned()
            } else if alloc_result == libc::EINVAL {
                "EINVAL".to_owned()
            } else {
                format!("code {alloc_result}")
            }
        )
    }

    let permset_result = libc::mprotect(
        *code_mem_ptr as *mut [u8] as *mut _,
        code.len(),
        libc::PROT_EXEC | libc::PROT_WRITE,
    );

    if permset_result != 0 {
        panic!(
            "code_alloc_inner: setting memory protection failed with {} (on size {alloc_size})",
            if permset_result == libc::ENOMEM {
                "ENOMEM".to_owned()
            } else if permset_result == libc::EINVAL {
                "EINVAL".to_owned()
            } else if permset_result == libc::EACCES {
                "EACCES".to_owned()
            } else {
                format!("code {permset_result}")
            }
        );
    }

    let code_mem = &mut (**code_mem_uninit.as_mut_ptr());

    //move the code to page aligned memory
    std::ptr::copy_nonoverlapping(
        code as *const [u8] as *const u8,
        code_mem as *mut [u8] as *mut u8,
        code.len(),
    );
    code_mem as *mut [u8] as *const ()
}

macro_rules! code_alloc {
    ($code:expr => $type:ty) => {
        unsafe { std::mem::transmute::<*const (), $type>(code_alloc_inner($code)) }
    };

    ($code:expr) => {
        unsafe { std::mem::transmute(code_alloc_inner($code)) }
    };
}

#[rustfmt::skip]
#[derive(Debug, Copy, Clone)]
enum GPReg {
    Al, Ax, Eax, Rax, Mm0, Xmm0, // R/M = 0
    Cl, Cx, Ecx, Rcx, Mm1, Xmm1, // R/M = 1
    Dl, Dx, Edx, Rdx, Mm2, Xmm2, // R/M = 2
    Bl, Bx, Ebx, Rbx, Mm3, Xmm3, // R/M = 3

    Ah, Sp, Esp, Rsp, Mm4, Xmm4, // R/M = 4
    Ch, Bp, Ebp, Rbp, Mm5, Xmm5, // R/M = 5
    Dh, Si, Esi, Rsi, Mm6, Xmm6, // R/M = 6
    Bh, Di, Edi, Rdi, Mm7, Xmm7, // R/M = 7
}

impl From<GPReg> for u8 {
    fn from(val: GPReg) -> Self {
        use crate::GPReg::*;
        match val {
            Al | Ax | Eax | Rax | Mm0 | Xmm0 => 0,
            Cl | Cx | Ecx | Rcx | Mm1 | Xmm1 => 1,
            Dl | Dx | Edx | Rdx | Mm2 | Xmm2 => 2,
            Bl | Bx | Ebx | Rbx | Mm3 | Xmm3 => 3,

            Ah | Sp | Esp | Rsp | Mm4 | Xmm4 => 4,
            Ch | Bp | Ebp | Rbp | Mm5 | Xmm5 => 5,
            Dh | Si | Esi | Rsi | Mm6 | Xmm6 => 6,
            Bh | Di | Edi | Rdi | Mm7 | Xmm7 => 7,
        }
    }
}

#[rustfmt::skip]
#[derive(Debug, Copy, Clone)]
enum Mod {
    RegAddr, Sib, Disp32,               //Mod = 0
    RegAddrPlusDisp8, SIBPlusDisp8,     //Mod = 1
    RegAddrPlusDisp32, SIBPlusDisp32,   //Mod = 2
    Reg                                 //Mod = 3
}

struct ModRM {
    modrm: ModRMsub,
    reg: GPReg,
}

#[derive(Debug, Copy, Clone)]
enum ModRMsub {
    RegAddr(GPReg),
    Disp32(u32),            //Mod = 0
    RegAddrPlusDisp32(u32), //Mod = 2
    Reg(GPReg),             //Mod = 3
}

impl From<Mod> for u8 {
    fn from(val: Mod) -> Self {
        use crate::Mod::*;
        match val {
            RegAddr | Sib | Disp32 => 0,
            RegAddrPlusDisp8 | SIBPlusDisp8 => 1,
            RegAddrPlusDisp32 | SIBPlusDisp32 => 2,
            Reg => 3,
        }
    }
}

#[derive(Clone, Debug)]
enum LabelField {
    Unresolved(Label),
    Resolved(i32),
}

impl LabelField {
    fn off32(&self) -> i32 {
        match self {
            Self::Resolved(rip) => *rip,
            _ => panic!("Unresolved RipField"),
        }
    }
    fn off8(&self) -> i8 {
        assert!(self.off32().abs() < 128);
        self.off32() as i8
    }
    fn new(label: &str) -> Self {
        Self::Unresolved(Label(label.to_string()))
    }
}

trait Encodable {
    fn encode(&self) -> Vec<u8>;
    fn len(&self) -> u32;

    fn has_label(&self) -> bool {
        false
    }
    fn get_label(&self) -> &str {
        unimplemented!("A labeled instructions must implement get_label() function");
    }

    fn need_reloff_resolving(&mut self) -> bool {
        false
    }
    fn resolve_reloff(&mut self) -> Vec<&mut LabelField> {
        unimplemented!(
            "An instruction that needs a relative offset must implement resolve_reloff() function"
        )
    }
}

struct ModRMImm {
    mod_val: Mod,
    reg: GPReg,
    rm: GPReg,
}

x86!(0xC3, Ret);

impl Encodable for u8 {
    fn encode(&self) -> Vec<u8> {
        vec![*self]
    }
    fn len(&self) -> u32 {
        self.encode().len() as u32
    }
}

enum OperandFieldMap {
    ModRM,
    ModRMDisp,
    Immediate,
}

fn gen_code(codes: &mut [Box<dyn Encodable>]) -> Vec<u8> {
    let mut labels_parsed: std::collections::HashMap<String, i32> =
        std::collections::HashMap::new();
    //label parsing pass
    codes.iter().fold(0, |pos, ins| {
        if ins.has_label() {
            labels_parsed.insert(ins.get_label().to_owned(), pos);
            pos
        } else {
            pos + ins.len() as i32
        }
    });

    let code_len = codes.len() * 8;

    //encoding
    codes
        .iter_mut()
        .fold(
            (Vec::with_capacity(code_len), 0),
            |(mut encoded, pos), code| {
                let code_len = code.len() as i32;
                if code.need_reloff_resolving() {
                    let labels = code.resolve_reloff();
                    for label in labels {
                        match label {
                            LabelField::Unresolved(Label(name)) => {
                                *label = LabelField::Resolved(
                                    *labels_parsed.get(name).unwrap_or_else(|| {
                                        panic!("Failed to resolve label '{name}'")
                                    }) - pos
                                        - code_len,
                                )
                            }
                            _ => (),
                        }
                    }
                }
                encoded.push(code.encode());
                (encoded, pos + code.len() as i32)
            },
        )
        .0
        .iter()
        .flatten()
        .copied()
        .collect::<Vec<u8>>()
}

fn code<T: Encodable>(code: T) -> Box<T> {
    Box::new(code)
}

x86! {Loop,
    0xE2, Rel8, start:LabelField => Rel8(start)
}

x86! {Mov,
    0x89, RR, dst:GPReg, src:GPReg => [ModRM(Mod::Reg, *src, *dst)]
    0xB8+u8::from(*dst), RImm, dst:GPReg, val:i32 => [Imm32(val)]
    0x89, MR, dst:GPReg, src:GPReg => [ModRM(Mod::RegAddr, *src, *dst)]
    0x88, RM, dst:GPReg, src:GPReg => [ModRM(Mod::RegAddr, *dst, *src)]
}

x86! {Mov,
    [0xF3, 0x0F, 0x10], SSrr, dst:GPReg, src:GPReg => [ModRM(Reg, *dst, *src)]
    [0xF3, 0x0F, 0x10], SSrm, dst:GPReg, src:GPReg => [ModRM(RegAddr, *dst, *src)]
    [VEX3b.hex0F.hexF3, 0x10], SSavx, dst:GPReg, src:GPReg => [ModRM(RegAddr,*dst, *src)]
}

use Mod::*;

x86!(Adc,
    0x15, EaxImm, val:u32 => [Imm32(*val)]
    0x81, RImm, op1:GPReg, val:u32 => [ModRM(Reg, 2, *op1) Imm32(val)]
    0x81, MImm, op1:GPReg, val:u32 => [ModRM(RegAddr, 2, *op1) Imm32(val)]

    0x13, RR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    0x13, RM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]
    0x11, MR, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op2, *op1)]
);

x86! {
    Add,
    0x05, EaxImm, val:i32 => [Imm32(*val)]
    0x81, RImm, op1: GPReg, val:u32 => [ModRM(Reg, 0, *op1), Imm32(val)]
    0x81, MImm, op1: GPReg, val:u32 => [ModRM(RegAddr, 0, *op1), Imm32(val)]

    0x01, RR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op2, *op1)]
    0x01, MR, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op2, *op1)]
    0x03, RM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]
}

x86! {Sub,
    0x2D, EaxImm, val:i32 => [Imm32(val)]
    0x81, RImm, op1:GPReg, val:i32 => [ModRM(Reg, 5, *op1) Imm32(val)]
    0x81, MImm, op1:GPReg, val:i32 => [ModRM(RegAddr, 5, *op1) Imm32(val)]

    0x29, RR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op2, *op1)]
    0x29, MR, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op2, *op1)]
    0x2B, RM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]
}

x86!(And,
    0x25, EaxImm, val:u32 => [Imm32(*val)]
    0x81, RImm, op1:GPReg, val:u32 => [ModRM(Reg, 4, *op1) Imm32(*val)]
    0x81, MImm, op1:GPReg, val:u32 => [ModRM(RegAddr, 4, *op1) Imm32(*val)]

    0x23, RR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    0x23, RM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]
    0x21, MR, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op2, *op1)]
);

x86! {Call,
    0xff, M, op1:GPReg => [ModRM(RegAddr, 2, *op1)]
    0xff, R, op1:GPReg => [ModRM(Reg, 2, *op1)]
    //TODO
}

x86!(0xf8, Clc);
x86!(0xfc, Cld);

x86! {Cmp,
    0x3D, EaxImm, val:u32 => [Imm32(*val)]
    0x81, RImm, op1:GPReg, val:i32 => [ModRM(Reg, 7, *op1) Imm32(*val)]
    0x81, MImm, op1:GPReg, val:i32 => [ModRM(RegAddr, 7, *op1) Imm32(*val)]

    0x39, RR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op2, *op1)]
    0x39, MR, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op2, *op1)]
    0x3B, RM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]
}

x86! {Cmov,
    [0x0f, 0x47], AR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x47], AM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x43], AeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x43], AeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x42], BR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x42], BM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x46], BeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x46], BeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x42], CR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x46], CM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x44], ER, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x44], EM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4f], GR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4f], GM, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]

    [0x0f, 0x4D], GeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4D], GeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4C], LR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4C], LM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4E], LeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4E], LeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x46], NaR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x46], NaM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x42], NaeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x42], NaeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x43], NbR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x43], NbM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x47], NbeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x47], NbeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x43], NcR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x43], NcM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x45], NeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x45], NeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4E], NgR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4E], NgM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4C], NgeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4C], NgeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4D], NlR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4D], NlM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4F], NleR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4F], NleM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x41], NoR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x41], NoM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4B], NpR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4B], NpM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x49], NsR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x49], NsM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x45], NzR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x45], NzM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x40], OR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x40], OM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4A], PR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4A], PM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4A], PeR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4A], PeM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x4B], PoR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x4B], PoM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x48], SR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x48], SM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [0x0f, 0x44], ZR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)]
    [0x0f, 0x44], ZM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]
}

x86! {Dec,
   0x48+u8::from(*op1), R, op1:GPReg;
   0xFF, M, op1:GPReg => [ModRM(RegAddr, 1, *op1)] //TODO: why 0x67?
}

x86! {Div,
    0xF7, R, op1:GPReg => [ModRM(Reg, 6, *op1)]
    0xF7, M, op1:GPReg => [ModRM(RegAddr, 6, *op1)]
}

x86! {IDiv,
    0xF7, R, op1:GPReg => [ModRM(Reg, 7, *op1)]
    0xF7, M, op1:GPReg => [ModRM(RegAddr, 7, *op1)]
}

x86! {IMul,
    0xF7, R, op1:GPReg => [ModRM(Reg, 5, *op1)]
    0xF7, M, op1:GPReg => [ModRM(RegAddr, 5, *op1)]

    0x69, ReqRxImm, op1:GPReg, op2:GPReg, val:i32 => [ModRM(Reg, *op1, *op2) Imm32(*val)]
    0x69, ReqMxImm, op1:GPReg, op2:GPReg, val:i32 => [ModRM(RegAddr, *op1, *op2) Imm32(*val)]
}

x86! {Inc,
    0x40+u8::from(*op1), R, op1:GPReg;
    0xFF, M, op1:GPReg => [ModRM(RegAddr, 0, *op1)]
}

//TODO x86!{LEA}

x86! {Jmp,
    0xFF, R, op1:GPReg => [ModRM(Reg, 4, *op1)]
    0xFF, M, op1:GPReg => [ModRM(RegAddr, 4, *op1)]
}

x86! {Jmp,
    [0xE9], Rel32, label:LabelField => [Rel32(label)]
}

x86! {Jmp,
    [0x0F, 0x87], ANear  ,label:LabelField => Rel32(label)
    [0x0F, 0x83], AENear ,label:LabelField => Rel32(label)
    [0x0F, 0x82], BNear  ,label:LabelField => Rel32(label)
    [0x0F, 0x86], BENear ,label:LabelField => Rel32(label)
    [0x0F, 0x82], CNear  ,label:LabelField => Rel32(label)
    [0x0F, 0x84], ENear  ,label:LabelField => Rel32(label)
    [0x0F, 0x8F], GNear  ,label:LabelField => Rel32(label)
    [0x0F, 0x8D], GENear ,label:LabelField => Rel32(label)
    [0x0F, 0x8C], LNear  ,label:LabelField => Rel32(label)
    [0x0F, 0x8e], LENear ,label:LabelField => Rel32(label)
    [0x0F, 0x86], NANear ,label:LabelField => Rel32(label)
    [0x0F, 0x82], NAENear,label:LabelField => Rel32(label)
    [0x0F, 0x83], NBNear ,label:LabelField => Rel32(label)
    [0x0F, 0x87], NBENear,label:LabelField => Rel32(label)
    [0x0F, 0x83], NCNear ,label:LabelField => Rel32(label)
    [0x0F, 0x85], NENear ,label:LabelField => Rel32(label)
    [0x0F, 0x8E], NGNear ,label:LabelField => Rel32(label)
    [0x0F, 0x8C], NGENear,label:LabelField => Rel32(label)
    [0x0F, 0x8D], NLNear ,label:LabelField => Rel32(label)
    [0x0F, 0x8F], NLENear,label:LabelField => Rel32(label)
    [0x0F, 0x81], NONear ,label:LabelField => Rel32(label)
    [0x0F, 0x8B], NPNear ,label:LabelField => Rel32(label)
    [0x0F, 0x89], NSNear ,label:LabelField => Rel32(label)
    [0x0F, 0x85], NZNear ,label:LabelField => Rel32(label)
    [0x0F, 0x80], ONear  ,label:LabelField => Rel32(label)
    [0x0F, 0x8A], PNear  ,label:LabelField => Rel32(label)
    [0x0F, 0x8A], PENear ,label:LabelField => Rel32(label)
    [0x0F, 0x8B], PONear ,label:LabelField => Rel32(label)
    [0x0F, 0x88], SNear  ,label:LabelField => Rel32(label)
    [0x0F, 0x84], ZNear  ,label:LabelField => Rel32(label)
}

x86! {Push,
    [0x50+u8::from(*op1)], R, op1:GPReg;
    [0xFF], M, op1:GPReg => [ModRM(RegAddr, 6, *op1)]
    [0x68], Imm, val:u32 => [Imm32(*val)]
    //TODO
}

x86! {Pop,
    [0x58+u8::from(*op1)], R, op1:GPReg;
    [0x8F], M, op1:GPReg => [ModRM(RegAddr, 0, *op1)]
    //TODO
}

#[derive(Debug, Clone)]
struct Label(String);

impl Label {
    fn new(name: &str) -> Self {
        Self(name.to_string())
    }
}

impl Encodable for Label {
    fn encode(&self) -> Vec<u8> {
        vec![]
    }
    fn has_label(&self) -> bool {
        true
    }
    fn get_label(&self) -> &str {
        &self.0
    }
    fn len(&self) -> u32 {
        self.encode().len() as u32
    }
}

x86! {Int,
    0xCC, Int3Bkp;
    0xCD, Imm, val:u8 => [Imm8(val)]
    0xCE, Int0Ovf;
    0xF1, Int1Dbg;
}

x86! {MovR64,
    [REX.W, 0x89], RR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op2, *op1)]
    [REX.W, 0x89], MR, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op2, *op1)]
    [REX.W, 0x8B], RM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]
}

fn modrm_raw_byte<P: Into<u8>, Q: Into<u8>, R: Into<u8>>(mod_val: P, reg_opcode: Q, rm: R) -> u8 {
    let mod_val = mod_val.into();
    let reg_opcode = reg_opcode.into();
    let rm = rm.into();

    assert!(mod_val <= 0b11);
    assert!(reg_opcode <= 0b111);
    assert!(rm <= 0b111);

    (mod_val << 6) + (reg_opcode << 3) + rm
}

macro_rules! code_vec{
    ($($code:expr),* $(,)?) => { vec![$(std::boxed::Box::new($code)),*] }
}

fn main() {
    use GPReg::*;

    let mut val2 = 20;

    let mut f = |i: i32| unsafe {
        libc::printf("Hi %d\n\0".as_ptr() as _, i);
        val2 = 42;
    };

    let mut code: Vec<Box<dyn Encodable>> = code_vec![
        MovR64RR(Rbx, Rdi),
        MovRImm(Ecx, 20),
        MovRImm(Eax, 0),
        MovSSrr(Xmm0, Xmm1),
        Label::new("loop_start"),
        // Add::RImm(Eax, 1),
        AddRImm(Eax, 0x2),
        // AddR(Eax, Eax),
        PushR(Rax),
        PushR(Rcx),
        MovR64RR(Edi, Ecx),
        IMulReqRxImm(Edi, Edi, 1),
        CallM(Ebx),
        PopR(Rcx),
        PopR(Rax),
        SubRImm(Ecx, 1),
        CmpRImm(Ecx, 0),
        JmpGENear(LabelField::new("loop_start")),
        Ret(),
    ];

    let code_encoded = gen_code(&mut code);

    use termion::color;
    use termion::style;

    std::fs::write("./code", &code_encoded);
    println!(
        "{}{}running ndisasm...{}{}",
        color::Fg(color::Cyan),
        style::Italic,
        style::Reset,
        color::Fg(color::Blue)
    );
    let _ = std::process::Command::new("ndisasm")
        .args(["-b 64", "./code"])
        .stdout(Stdio::inherit())
        .output()
        .unwrap();
    println!("{}", color::Fg(color::Reset));

    let fc = ClosureMut1::new(&mut f);
    let fc = fc.code_ptr();
    let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };
    let code_fn = code_alloc!(&code_encoded => extern "C" fn(extern "C" fn(i32) -> ()) -> u64);
    let result = code_fn(fc_ptr);
    println!("result = {result}");
}
