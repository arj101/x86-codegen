use codegen::pretty_code_vec;
use codegen::x86;
use libffi::high::ClosureMut1;
use std::{mem::MaybeUninit, process::Stdio};

use libffi::high::Closure1;
use std::ffi::CStr;
use crate::instructions::Encodable2;

use ::core::ptr;
use ::std::alloc;

#[repr(transparent)]
pub struct InsPtr(pub Box<dyn Encodable2>);

impl Drop for InsPtr {
    fn drop(&mut self) {
        // println!("dropped InsPtr");
    }
}


pub unsafe fn code_alloc_inner(code: &[u8]) -> *const () {
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

    allocated.as_ptr() as *const ()
}

#[macro_export]
macro_rules! code_alloc {
    ($code:expr => $type:ty) => {
        unsafe { std::mem::transmute::<*const (), $type>((crate::encode::code_alloc_inner($code))) }
    };

    ($code:expr) => {
        unsafe { std::mem::transmute(crate::encode::code_alloc_inner($code)) }
    };
}

macro_rules! encode {
    ($code:expr => $rt_type:ty) => {
        {
        let code_encoded = gen_code($code);

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
            .stdout(std::process::Stdio::inherit())
            .output()
            .unwrap();
        println!("{}", color::Fg(color::Reset));

        // let fn_mut_ref = &mut f;
        // let fc_ = libffi::high::Closure1::new(&mut f);
        // let fc = fc_.code_ptr()u

        // let fc_ptr: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(fc) };
        // let code_ptr = unsafe { code_alloc_inner(&code_encoded.0) };
        // let code_entry_ptr = unsafe { &((*code_ptr.0)[0]) };

        // let code_fn: extern "C" fn (extern "C" fn(i32) -> i32) -> u64 = unsafe { std::mem::transmute(code_entry_ptr) };
        let code_fn = crate::code_alloc!(&code_encoded.0 => $rt_type);

        // unsafe {
        //     let s = &(*code_ptr);
        // let layout = alloc::Layout::from_size_align(s.len(), 4096).expect("layout error");
        // alloc::dealloc(code_encode, layout);
        // }
        code_fn
        }
    };
    (no_disasm, $code:expr => $rt_type:ty) => {
        {
        let code_encoded = gen_code($code);

        // let fn_mut_ref = &mut f;
        // let fc_ = libffi::high::Closure1::new(&mut f);
        // let fc = fc_.code_ptr()u

        // let fc_ptr: extern "C" fn(i32) -> i32 = unsafe { std::mem::transmute(fc) };
        // let code_ptr = unsafe { code_alloc_inner(&code_encoded.0) };
        // let code_entry_ptr = unsafe { &((*code_ptr.0)[0]) };
        // let code_fn: extern "C" fn (extern "Ca" fn(i32) -> i32) -> u64 = unsafe { std::mem::transmute(code_entry_ptr) };
        let code_fn = crate::code_alloc!(&code_encoded.0 => $rt_type);

        // unsafe {
        //     let s = &(*code_ptr);
        // let layout = alloc::Layout::from_size_align(s.len(), 4096).expect("layout error");
        // alloc::dealloc(code_encode, layout);
        // }
        code_fn
        }
    };
}
