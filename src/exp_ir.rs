use codegen::pretty_code_vec;

use crate::encode::InsPtr;
use crate::instructions::GPReg::*;
use crate::instructions::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum UnOp {
    Not,
    Neg,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum RegLoc {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
}

enum MemoryLoc {
    StackLoc(StackVal),
    Reg(RegLoc),
}

struct RegisterAllocator {
    available_regs: Vec<RegLoc>,
    used_regs: Vec<RegLoc>,
}

impl RegisterAllocator {
    fn new() -> Self {
        RegisterAllocator {
            available_regs: vec![
                RegLoc::Rax,
                RegLoc::Rbx,
                RegLoc::Rcx,
                RegLoc::Rdx,
                RegLoc::Rsi,
                RegLoc::Rdi,
            ],
            used_regs: vec![],
        }
    }

    fn alloc_reg(&mut self) -> RegLoc {
        let reg = self.available_regs.pop().unwrap();
        self.used_regs.push(reg);
        reg
    }

    fn alloc_reg_named(&mut self, reg: &RegLoc) -> Result<RegLoc, ()> {
        if self.available_regs.contains(reg) {
            self.available_regs.retain(|r| r != reg);
            self.used_regs.push(*reg);
            Ok(*reg)
        } else {
            Err(())
        }
    }

    fn free_reg(&mut self, reg: RegLoc) {
        self.used_regs.retain(|r| *r != reg);
        self.available_regs.push(reg);
    }
}

#[derive(Clone, Copy)]
struct StackVal {
    offset: u32,
    size: u32,
}

//A memory allocation tracker based on RBP offset
struct StackAllocator {
    stack_map: HashMap<Rc<String>, StackVal>,
    alloc_gaps: Vec<StackVal>,
    stack_offset: u32,
}

impl StackAllocator {
    fn new() -> Self {
        StackAllocator {
            stack_map: HashMap::new(),
            stack_offset: 0,
            alloc_gaps: Vec::new(),
        }
    }

    fn alloc_val(&mut self, ident: Rc<String>, size: u32) -> u32 {
        let mut rbp_offset = self.stack_offset + size;

        if self.alloc_gaps.len() > 0 {
            let mut idx: Option<usize> = None;
            for (i, gap) in self.alloc_gaps.iter().enumerate() {
                if gap.size >= size {
                    rbp_offset = gap.offset;
                    idx = Some(i);
                    break;
                }
            }
            if let Some(idx) = idx {
                self.alloc_gaps.remove(idx);
            }
        }

        self.stack_map.insert(
            ident,
            StackVal {
                offset: rbp_offset,
                size,
            },
        );
        self.stack_offset += size;
        rbp_offset
    }

    fn get_val(&self, ident: &Rc<String>) -> u32 {
        self.stack_map.get(ident).unwrap().offset
    }

    fn get_stack_val(&self, ident: &Rc<String>) -> StackVal {
        self.stack_map.get(ident).unwrap().clone()
    }

    fn get_val_size(&self, ident: &Rc<String>) -> u32 {
        self.stack_map.get(ident).unwrap().size
    }

    fn free_val(&mut self, ident: &Rc<String>) {
        if !self.stack_map.contains_key(ident) {
            return;
        }

        let val = self.stack_map.remove(ident).unwrap();
        if val.offset == self.stack_offset {
            self.stack_offset -= val.size;
        } else {
            self.alloc_gaps.push(val);
        }
    }
}

struct IREnv {
    intermediate_name_counter: u32,
    stack_alloc: StackAllocator,
    reg_alloc: RegisterAllocator,
}

struct StackIntermediateValPtr {
    name: Rc<String>,
    offset: u32,
    size: u32,
}

impl Into<MemoryLoc> for StackIntermediateValPtr {
    fn into(self) -> MemoryLoc {
        MemoryLoc::StackLoc(StackVal {
            offset: self.offset,
            size: self.size,
        })
    }
}

impl Into<MemoryLoc> for StackVal {
    fn into(self) -> MemoryLoc {
        MemoryLoc::StackLoc(self)
    }
}

impl Into<MemoryLoc> for RegLoc {
    fn into(self) -> MemoryLoc {
        MemoryLoc::Reg(self)
    }
}

impl Into<StackVal> for StackIntermediateValPtr {
    fn into(self) -> StackVal {
        StackVal {
            offset: self.offset,
            size: self.size,
        }
    }
}

impl Into<Rc<String>> for StackIntermediateValPtr {
    fn into(self) -> Rc<String> {
        self.name
    }
}

impl IREnv {
    fn new() -> Self {
        IREnv {
            intermediate_name_counter: 0,
            stack_alloc: StackAllocator::new(),
            reg_alloc: RegisterAllocator::new(),
        }
    }

    fn alloc_stack(&mut self, name: Rc<String>, val: i32, size: u32) -> MemoryLoc {
        let offset = self.stack_alloc.alloc_val(name, size);
        MemoryLoc::StackLoc(StackVal { offset, size })
    }

    fn dealloc_stack(&mut self, name: Rc<String>) {
        self.stack_alloc.free_val(&name);
    }

    fn alloc_intermediate_val(&mut self, val: i32, size: u32) -> StackIntermediateValPtr {
        let name = Rc::new(format!("__intermediate_{}", self.intermediate_name_counter));
        self.intermediate_name_counter += 1;
        let offset = self.stack_alloc.alloc_val(name.clone(), size);
        StackIntermediateValPtr { name, offset, size }
    }
}

type Instructions = Vec<InsPtr>;

pub trait ExpIR {
    fn eval(&mut self, env: &mut IREnv) -> Result<(Instructions, MemoryLoc), String>;
}

struct Literal(i32);

//evaluating a literal stores the value on stack(or register in the future!) and returns the location
impl ExpIR for Literal {
    fn eval(&mut self, env: &mut IREnv) -> Result<(Instructions, MemoryLoc), String> {
        let val = env.alloc_intermediate_val(self.0, 4);
        let offset = val.offset as i32;
        let offset = -offset;
        let code = pretty_code_vec!(
            Mov64Md32Imm32 Rbp offset self.0 ;
        );
        todo!()
    }
}

struct Ident(Rc<String>);
//evaluating an identifier only retrieves the offset from the stack map. No code generation is needed
impl ExpIR for Ident {
    fn eval(&mut self, env: &mut IREnv) -> Result<(Instructions, MemoryLoc), String> {
        let loc = env.get_stack_loc(&self.0);
        Ok((vec![], loc))
    }
}
