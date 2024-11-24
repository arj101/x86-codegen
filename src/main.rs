#![allow(dead_code, unused_variables, path_statements, unused_must_use)]

mod instructions;
#[macro_use]
mod encode;
mod exp_ir;
mod ir;

use crate::encode::{code_alloc_inner, InsPtr};
use crate::instructions::{GPReg::*, *};
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::process::Stdio;

use codegen::pretty_code_vec;

pub fn quick_run<A, R>(arg: A, code: Vec<InsPtr>) -> R {
    let f = encode!(code => extern "C" fn(A) -> R);
    f(arg)
}

use ir::*;
use std::rc::Rc;

extern "C" fn ree(i: i32) {
    println!("hello world {i}");
}

fn main() {
    // let mut f = |i: i32| -> i32 {
    //     println!("hi from rust {i}");
    //     i + 1
    // };
    //
    // let fn_mut_ref = &mut f;
    // let fc_ = libffi::high::Closure1::new(&mut f);
    // let fc = fc_.code_ptr();
    // let fc_ptr: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(fc) };
    //
    // let code = pretty_code_vec![
    //     // PushR(Rax),
    //     PushR Rbx;
    //     PushR Rcx;
    //     PushR Rdx;
    //     PushR Rsi;
    //     PushR Rdi;
    //
    //     PushR Rbp;
    //     Mov64RR Rbp Rsp;
    //     //
    //     Mov64RR Rbx Rdi;
    //
    //     Mov64RImm64 Rax 284242849438492;
    //     Mov64Md8R Rbp -2 Rax;
    //
    //     Mov64Md8Imm32 Rbp -4 1;
    //
    //     $loop_start
    //
    //     Mov64RMd8 Rdi Rbp -4;
    //     CallM Rbx;
    //     Mov64Md8R Rbp -4 Rax;
    //
    //     CmpRImm Rax 10;
    //     JmpLENear $loop_start;
    //
    //     // MovssRM Xmm1 Rax;
    //
    //     Mov64RImm64 Rax 2;
    //
    //     Mov64RMrel32off Rax $num 0; //return the value stores in memory address num
    //
    //     PopR Rbp;
    //     PopR Rdi;
    //     PopR Rsi;
    //     PopR Rdx;
    //     PopR Rcx;
    //     PopR Rbx;
    //     // PopR(Rax),
    //     Ret;
    //
    //     @
    //        AlignmentPadding(8)
    //        //Adds padding so that the next byte is aligned as per the
    //        //given alginemnt
    //     @
    //
    //     $num
    //     @
    //        1234u64.to_le_bytes().to_vec()
    //        0xddccbbaa9988u64.to_le_bytes().to_vec()
    //        0x77665544332211u64.to_le_bytes().to_vec()
    //     @
    // ];
    //
    // // let code_fn = encode!(cod => extern "C" fn ( extern "C" fn (i32) -> i32) -> i64);
    // // let result = code_fn(ree);
    // println!(
    //     "result = {}",
    //     quick_run::<extern "C" fn(i32) -> i32, i32>(fc_ptr, code)
    // );

    let mut output = vec![];
    let oref = &mut output;
    let mut f = |i: i32| {
        println!(">>> {i}");
        oref.push(i);
    };

    let fn_mut_ref = &mut f;
    let fc_ = libffi::high::ClosureMut1::new(&mut f);
    let fc = fc_.code_ptr();
    let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

    macro_rules! ident {
        ($s:expr) => {
            Rc::new(($s).to_string())
        };
    };

    macro_rules! ident_val {
        ($s:expr) => {
            Val::Ident(Rc::new(($s).to_string()))
        };
    };

    macro_rules! li32 {
        ($s:expr) => {
            Val::Literal($s)
        };
    };

    let inss = ir_encode_fn(vec![
        IRIns::InitStore32 {
            dest: ident!("hello0"),
            val: -10,
        },
        IRIns::InitStore32 {
            dest: ident!("hello1"),
            val: -11,
        },
        IRIns::Print32 {
            val: ident_val!("hello0"),
        },
        IRIns::Mul32 {
            dest: ident!("hello0"),
            val1: ident_val!("hello0"),
            val2: li32!(-124),
        },
        IRIns::Print32 {
            val: ident_val!("hello0"),
        },
        IRIns::Mul32 {
            dest: ident!("hello0"),
            val2: li32!(-200),
            val1: ident_val!("hello0"),
        },
        IRIns::Print32 {
            val: ident_val!("hello0"),
        },
        IRIns::Div32 {
            dest: ident!("hello0"),
            val1: ident_val!("hello1"),
            val2: li32!(2),
        },
        IRIns::Print32 {
            val: ident_val!("hello0"),
        },
    ]);

    let f = encode!(inss => extern "C" fn(extern "C" fn (i32) -> ()) -> i32);
    let result = f(fc_ptr);

    drop(fc_);

    println!("outputs = {output:?}");
}
