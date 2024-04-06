#![allow(dead_code, unused_variables, path_statements, unused_must_use)]

mod instructions;

use instructions::{GPReg::*, *};

use codegen::x86;
use libffi::high::ClosureMut1;
use std::{mem::MaybeUninit, process::Stdio};

use ::core::ptr;
use ::std::alloc;

pub struct CodePtr(*mut [u8]);

impl Drop for CodePtr {
    fn drop(&mut self) {
        // println!("Dropped code");
        // self.0.drop_in_place();
    }
}

unsafe fn code_alloc_inner(code: &[u8]) -> CodePtr {
    let alloc_size = code.len().max(4096);

    const PAGE_SIZE: usize = 4096;
    let layout = alloc::Layout::from_size_align(alloc_size, PAGE_SIZE).expect("layout error");
    let allocated = ptr::NonNull::new(alloc::alloc_zeroed(layout))
        .map(|p| p.cast::<()>())
        .expect("allocation failed");


    let permset_result = libc::mprotect(
        allocated.as_ptr() as *mut _,
        code.len(),
        libc::PROT_EXEC | libc::PROT_WRITE | libc::PROT_READ,
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

    // move the code to page aligned memory
    std::ptr::copy_nonoverlapping(
        code as *const [u8] as *const u8,
        allocated.as_ptr() as *mut u8,
        code.len(),
    );
 
    CodePtr(std::ptr::slice_from_raw_parts_mut(allocated.as_ptr() as *mut u8, code.len()))
}

macro_rules! code_alloc {
    ($code:expr => $type:ty) => {
        unsafe { std::mem::transmute::<*mut u8, $type>((*code_alloc_inner($code)).as_mut_ptr()) }
    };

    ($code:expr) => {
        unsafe { std::mem::transmute(code_alloc_inner($code)) }
    };
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
    ($($code:expr),* $(,)?) => { vec![$(InsPtr(std::boxed::Box::new($code))),*] }
}

use libffi::high::Closure1;
use std::ffi::CStr;

fn f(i: i32) {

    //     let mut s = String::from("EE");
    //     println!("{s}");
    //
    // std::mem::forget(s);
    // val2 = 2;
}

pub struct InsPtr(pub Box<dyn Encodable>);

impl Drop for InsPtr {
    fn drop(&mut self) {
        // println!("dropped InsPtr");
    }
}

extern "C" fn aaa(i: i32)  {
        use std::io::Write;
        write!(std::io::stdout(), "HI");
}


fn main() {
    let mut val2 = 20;

    let mut f = |i: i32| {
        use std::io::Write;
        println!("hi from rust {i}");
        return i + 5;
    };

    let code: Vec<InsPtr> = code_vec![
        PushR(Rax),
        PushR(Rbx),
        PushR(Rcx),
        PushR(Rdx),
        PushR(Rsi),
        PushR(Rdi),

        PushR(Rbp),
        Mov64RR(Rbp, Rsp),

        // Mov64RR(Rbx, Rdi),
        // MovRImm(Ecx, 20),
        // MovRImm(Eax, 0),
        // MovSSrr(Xmm0, Xmm1),
        // Label::new("loop_start"),
        // AddRImm(Eax, 0x2),
        // PushR(Rax),
        // PushR(Rcx),
        // Mov64RR(Edi, Ecx),
        // IMulReqRxImm(Edi, Edi, 1),
        // MovRR(Edi, Rdi),
        // CallM(Edx),
        // Mov64RMd8(Edi, Rdi, -4),
        // PopR(Rcx),
        // PopR(Rax),
        // SubRImm(Ecx, 1),
        // CmpRImm(Ecx, 0),
        // JmpGENear(LabelField::new("loop_start")),
        // MovRImm(Rax, 2),
        // Mov64RR(Rsp, Rbp),
        
        Mov64Md8Imm32(Rbp, 16, 4),


        PopR(Rbp),

        PopR(Rdi),
        PopR(Rsi),
        PopR(Rdx),
        PopR(Rcx),
        PopR(Rbx),
        PopR(Rax),
        Ret(),
    ];

    let code_encoded = gen_code(code);

    use termion::color;
    use termion::style;

    std::fs::write("./code", &code_encoded.0);
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

    let fn_mut_ref = &mut f;
    let fc_ = libffi::high::Closure1::new(&mut f);
    let fc = fc_.code_ptr();

    let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };
    let code_ptr = unsafe { code_alloc_inner(&code_encoded.0) };
    let code_entry_ptr = unsafe { &((*code_ptr.0)[0]) };
    
    let code_fn: extern "C" fn (extern "C" fn(i32)) -> u64 = unsafe { std::mem::transmute(code_entry_ptr) };

    let result = code_fn(fc_ptr);
    // unsafe {
    //     let s = &(*code_ptr);
    // let layout = alloc::Layout::from_size_align(s.len(), 4096).expect("layout error");
    // alloc::dealloc(code_encode, layout);
    // }
    println!("result = {result}");

    // std::mem::forget(code_encoded);
    // std::mem::forget(code);
    // std::mem::forget(fc_);
}
