use std::io::Write;

use codegen::x86;

#[rustfmt::skip]
#[derive(Debug, Copy, Clone)]
pub enum GPReg {
    Al, Ax, Eax, Rax, Mm0, Xmm0, // R/M = 0
    Cl, Cx, Ecx, Rcx, Mm1, Xmm1, // R/M = 1
    Dl, Dx, Edx, Rdx, Mm2, Xmm2, // R/M = 2
    Bl, Bx, Ebx, Rbx, Mm3, Xmm3, // R/M = 3

    Ah, Sp, Esp, Rsp, Mm4, Xmm4, // R/M = 4
    Ch, Bp, Ebp, Rbp, Mm5, Xmm5, // R/M = 5
    Dh, Si, Esi, Rsi, Mm6, Xmm6, // R/M = 6
    Bh, Di, Edi, Rdi, Mm7, Xmm7, // R/M = 7
    //
    R8b, R8w, R8d, R8,
    R9b, R9w, R9d, R9,
    R10b, R10w, R10d, R10,
    R11b, R11w, R11d, R11,
    R12b, R12w, R12d, R12,
    R13b, R13w, R13d, R13,
    R14b, R14w, R14d, R14,
    R15b, R15w, R15d, R15,

    ///0b101, for handling a special case in ModRM byte
    None,
}

impl From<GPReg> for u8 {
    fn from(val: GPReg) -> Self {
        use GPReg::*;
        match val {
            Al | Ax | Eax | Rax | Mm0 | Xmm0 => 0,
            Cl | Cx | Ecx | Rcx | Mm1 | Xmm1 => 1,
            Dl | Dx | Edx | Rdx | Mm2 | Xmm2 => 2,
            Bl | Bx | Ebx | Rbx | Mm3 | Xmm3 => 3,

            Ah | Sp | Esp | Rsp | Mm4 | Xmm4 => 4,
            Ch | Bp | Ebp | Rbp | Mm5 | Xmm5 => 5,
            Dh | Si | Esi | Rsi | Mm6 | Xmm6 => 6,
            Bh | Di | Edi | Rdi | Mm7 | Xmm7 => 7,

            R8b | R8w | R8d | R8 => 0,
            R9b | R9w | R9d | R9 => 1,
            R10b | R10w | R10d | R10 => 2,
            R11b | R11w | R11d | R11 => 3,
            R12b | R12w | R12d | R12 => 4,
            R13b | R13w | R13d | R13 => 5,
            R14b | R14w | R14d | R14 => 6,
            R15b | R15w | R15d | R15 => 7,

            None => 0b101
        }
    }
}

impl GPReg {
    pub fn extended<T: From<bool>>(&self) -> T {
        use GPReg::*;
        match self {
            R8b | R8w | R8d | R8 | R9b | R9w | R9d | R9 | R10b | R10w | R10d | R10 | R11b
            | R11w | R11d | R11 | R12b | R12w | R12d | R12 | R13b | R13w | R13d | R13 | R14b
            | R14w | R14d | R14 | R15b | R15w | R15d | R15 => true,
            _ => false,
        }
        .into()
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
        use Mod::*;
        match val {
            RegAddr | Sib | Disp32 => 0,
            RegAddrPlusDisp8 | SIBPlusDisp8 => 1,
            RegAddrPlusDisp32 | SIBPlusDisp32 => 2,
            Reg => 3,
        }
    }
}

#[derive(Clone, Debug)]
pub enum LabelField {
    Unresolved(Label),
    Resolved(i32),
}

impl LabelField {
    pub fn off32(&self) -> i32 {
        match self {
            Self::Resolved(rip) => *rip,
            _ => panic!("Unresolved RipField"),
        }
    }
    pub fn off8(&self) -> i8 {
        assert!(self.off32().abs() < 128);
        self.off32() as i8
    }
    pub fn new(label: &str) -> Self {
        Self::Unresolved(Label(label.to_string()))
    }
}

pub trait Encodable {
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

pub struct CodeBuffer {
    code: Vec<u8>,
    pos: usize,
}


impl CodeBuffer {
    pub fn new() -> Self {
        Self { code: vec![], pos: 0 }
    }
    pub fn with_initial_size(size: usize) -> Self {
        Self {
            code: Vec::with_capacity(size),
            pos: 0,
        }
    }
    pub fn pos(&self) -> usize {
        self.pos
    }
}

impl Write for CodeBuffer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.code.extend(buf);
        self.pos += buf.len();
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub trait Encodable2 {
    fn encode(&self, code_buf: &mut CodeBuffer);
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

    fn has_dynamic_length(&self) -> bool {
        false
    }
    fn dynamic_length(&self, codepos: usize) -> u32 {
        unimplemented!(
           "A dynamically sized instruction must implement dynamic_length() function"
        )
    }
}

struct ModRMImm {
    mod_val: Mod,
    reg: GPReg,
    rm: GPReg,
}

x86!(0xC3, Ret);

// impl Encodable for u8 {
//     fn encode(&self) -> Vec<u8> {
//         vec![*self]
//     }
//     fn len(&self) -> u32 {
//         self.encode().len() as u32
//     }
// }

impl Encodable2 for u8 {
    fn encode(&self, buf: &mut CodeBuffer) {
        buf.write(&[*self]);
    }
    fn len(&self) -> u32 {
        1
    }
}

impl Encodable2 for &[u8] {
    fn encode(&self, buf: &mut CodeBuffer) {
        buf.write(self);
    }
    fn len(&self) -> u32 {
        std::mem::size_of_val(self) as u32
    }
}

enum OperandFieldMap {
    ModRM,
    ModRMDisp,
    Immediate,
}

pub struct GenCode(pub Vec<u8>);

impl Drop for GenCode {
    fn drop(&mut self) {
        // println!("Dropped GenCode");
    }
}

pub fn gen_code(mut codes: Vec<crate::InsPtr>) -> GenCode {
    let mut labels_parsed: std::collections::HashMap<String, i32> =
        std::collections::HashMap::new();
    //label parsing pass
    codes.iter().fold(0, |pos, ins| {
        if ins.0.has_label() {
            labels_parsed.insert(ins.0.get_label().to_owned(), pos);
            pos
        } else {
            let len = if ins.0.has_dynamic_length() { ins.0.dynamic_length(pos as usize) } else { ins.0.len() };
            pos + len as i32
        }
    });

    let code_len = codes.len() * 8;

    let mut code_buffer = CodeBuffer::new();


    let mut pos = 0;
    for code in &mut codes {
        let code_len = if code.0.has_dynamic_length() {
            code.0.dynamic_length(pos as usize)
        } else {
            code.0.len()
        };
        let code_len = code_len as i32;
        if code.0.need_reloff_resolving() {
            let labels = code.0.resolve_reloff();
            for label in labels {
                match label {
                    LabelField::Unresolved(Label(name)) => {
                        *label = LabelField::Resolved(
                            *labels_parsed
                                .get(name)
                                .unwrap_or_else(|| panic!("Failed to resolve label '{name}'"))
                                - pos
                                - code_len,
                        )
                    }
                    _ => (),
                }
            }
        }
        code.0.encode(&mut code_buffer);
        pos += code_len;
    }

    //encoding
    // GenCode(codes
    //     .iter_mut()
    //     .fold(
    //         (Vec::with_capacity(code_len), 0),
    //         |(mut encoded, pos), code| {
    //             let code_len = code.0.len() as i32;
    //             if code.0.need_reloff_resolving() {
    //                 let labels = code.0.resolve_reloff();
    //                 for label in labels {
    //                     match label {
    //                         LabelField::Unresolved(Label(name)) => {
    //                             *label = LabelField::Resolved(
    //                                 *labels_parsed.get(name).unwrap_or_else(|| {
    //                                     panic!("Failed to resolve label '{name}'")
    //                                 }) - pos
    //                                     - code_len,
    //                             )
    //                         }
    //                         _ => (),
    //                     }
    //                 }
    //             }
    //             encoded.push(code.0.encode());
    //             (encoded, pos + code.0.len() as i32)
    //         },
    //     )
    //     .0
    //     .iter()
    //     .flatten()
    //     .copied()
    //     .collect::<Vec<u8>>())
    GenCode(code_buffer.code)
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
pub struct Label(String);

impl Label {
    pub fn new(name: &str) -> Self {
        Self(name.to_string())
    }
}

// impl Encodable for Label {
//     fn encode(&self) -> Vec<u8> {
//         vec![]
//     }
//     fn has_label(&self) -> bool {
//         true
//     }
//     fn get_label(&self) -> &str {
//         &self.0
//     }
//     fn len(&self) -> u32 {
//         self.encode().len() as u32
//     }
// }ins
//
//
impl Encodable2 for Vec<u8> {
    fn encode(&self, buf: &mut CodeBuffer) {
        buf.write(self);
    }
    fn len(&self) -> u32 {
        std::mem::size_of_val(self) as u32
    }
}

pub struct AlignmentPadding(pub u32);


impl Encodable2 for AlignmentPadding {
    fn encode(&self, buf: &mut CodeBuffer) {
        let mut padding_needed = buf.pos % self.0 as usize;
        if padding_needed > 0 { padding_needed = self.0 as usize - padding_needed };

        for _ in 0..padding_needed {
            buf.write(&[0]);
        }
    }

    fn has_dynamic_length(&self) -> bool {
        true
    }

    fn dynamic_length(&self, pos: usize) -> u32 {
        let mut padding_needed = pos % self.0 as usize;
        if padding_needed > 0 { padding_needed = self.0 as usize - padding_needed };
        padding_needed as u32
    }

    fn len(&self) -> u32 {
        panic!("AlginmentPadding does not have static length. Tried to call .len() on a dynamically sized instruction");
    }
}

impl Encodable2 for Label {
    fn encode(&self, buf: &mut CodeBuffer) {}
    fn has_label(&self) -> bool {
        true
    }
    fn get_label(&self) -> &str {
        &self.0
    }
    fn len(&self) -> u32 {
        0
    }
}

x86! {Int,
    0xCC, Int3Bkp;
    0xCD, Imm, val:u8 => [Imm8(val)]
    0xCE, Int0Ovf;
    0xF1, Int1Dbg;
}

x86! {Mov64,
    [REX.W.(B=op1.extended::<u8>()).(R=op2.extended::<u8>()), 0x89], RR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op2, *op1)]
    [REX.W.(B=op1.extended::<u8>()).(R=op2.extended::<u8>()), 0x89], MR, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op2, *op1)]
    [REX.W.(B=op2.extended::<u8>()).(R=op1.extended::<u8>()), 0x8B], RM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)]

    [REX.W.(B=op2.extended::<u8>()).(R=op1.extended::<u8>()), 0x8B], RMd8, op1:GPReg, op2:GPReg, op3:i8 => [ModRM(RegAddrPlusDisp8, *op1, *op2), Imm8(op3)]

    [REX.W.(B=op2.extended::<u8>()).(R=op1.extended::<u8>()), 0x8B], RMd32, op1:GPReg, op2:GPReg, op3:i32 => [ModRM(RegAddrPlusDisp32, *op1, *op2), Imm32(op3)]

    [REX.W.(B=op2.extended::<u8>()).(R=op2.extended::<u8>()), 0x8B], RMrel32,  op2:GPReg, op3:LabelField => [ModRM(RegAddr, *op2, GPReg::None), Rel32(op3)]
    [REX.W.(B=op2.extended::<u8>()).(R=op2.extended::<u8>()), 0x8B], RMrel32off,  op2:GPReg, op3:LabelField, op4:i32 => [ModRM(RegAddr, *op2, GPReg::None), Rel32(op3, op4)]


    [REX.W.(B=op1.extended::<u8>()).(R=op3.extended::<u8>()), 0x89], Md8R, op1:GPReg, op2:i8, op3:GPReg => [ModRM(RegAddrPlusDisp8, *op3, *op1), Imm8(op2)]

    [REX.W.(B=op1.extended::<u8>()).(R=op3.extended::<u8>()), 0x89], Md32R, op1:GPReg, op2:i32, op3:GPReg => [ModRM(RegAddrPlusDisp32, *op3, *op1), Imm32(op2)]

    [REX.W.(B=op1.extended::<u8>()), 0xC7], Md8Imm32, op1:GPReg, op2: i32, op3: i32 => [ModRM(RegAddrPlusDisp8, 0, *op1), Imm8(op2), Imm32(op3)]

    [REX.W.(B=op1.extended::<u8>()), 0xC7], Md32Imm32, op1:GPReg, op2: i32, op3: i32 => [ModRM(RegAddrPlusDisp32, 0, *op1), Imm32(op2), Imm32(op3)]


    [REX.W.(B=op1.extended::<u8>()), 0xB8+u8::from(*op1)], RImm64, op1:GPReg, op2: i64 => [Imm64(op2)]
}

x86! {Movss,
    [0xF3, 0x0F, 0x10], RR, op1:GPReg, op2:GPReg => [ModRM(Reg, *op1, *op2)] 
    [0xF3, 0x0F, 0x10], RM, op1:GPReg, op2:GPReg => [ModRM(RegAddr, *op1, *op2)] 
}
