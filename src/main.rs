#![allow(dead_code, unused_variables, path_statements, unused_must_use)]

mod instructions;
#[macro_use]
mod encode;
mod ir;

use crate::encode::{code_alloc_inner, InsPtr};
use crate::instructions::{GPReg::*, *};
use std::process::Stdio;

use codegen::pretty_code_vec;

pub fn quick_run<A, R>(arg: A, code: Vec<InsPtr>) -> R {
    let f = encode!(code => extern "C" fn(A) -> R);
    f(arg)
}

fn main() {

    let mut f = |i: i32| -> i32 {
        println!("hi from rust {i}");
        i + 1
    };

    let fn_mut_ref = &mut f;
    let fc_ = libffi::high::Closure1::new(&mut f);
    let fc = fc_.code_ptr();
    let fc_ptr: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(fc) };

    let code = pretty_code_vec![
        // PushR(Rax),
        PushR Rbx;
        PushR Rcx;
        PushR Rdx;
        PushR Rsi;
        PushR Rdi;

        PushR Rbp;
        Mov64RR Rbp Rsp;
        //
        Mov64RR Rbx Rdi;

        Mov64RImm64 Rax 284242849438492;
        Mov64Md8R Rbp -2 Rax;

        Mov64Md8Imm32 Rbp -4 1;

        $loop_start

        Mov64RMd8 Rdi Rbp -4;
        CallM Rbx;
        Mov64Md8R Rbp -4 Rax;

        CmpRImm Rax 10;
        JmpLENear $loop_start;

        // MovssRM Xmm1 Rax;

        Mov64RImm64 Rax 2;

        Mov64RMrel32off Rax $num 0; //return the value stores in memory address num

        PopR Rbp;
        PopR Rdi;
        PopR Rsi;
        PopR Rdx;
        PopR Rcx;
        PopR Rbx;
        // PopR(Rax),
        Ret;

        @
           AlignmentPadding(8)
           //Adds padding so that the next byte is aligned as per the
           //given alginemnt
        @

        $num
        @
           1234u64.to_le_bytes().to_vec()
           0xddccbbaa9988u64.to_le_bytes().to_vec()
           0x77665544332211u64.to_le_bytes().to_vec()
        @
    ];

    // let code_fn = encode!(code => extern "C" fn ( extern "C" fn (i32) -> i32) -> i64);
    // let result = code_fn(ree);
    println!(
        "result = {}",
        quick_run::<extern "C" fn(i32) -> i32, i32>(fc_ptr, code)
    );
}
