use crate::instructions::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

enum UnOp {
    Not,
    Neg,
}

enum RegLoc {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
}

enum MemoryLoc {
    RbpOffset(i32),
    Reg(RegLoc),
}

pub enum ExpIR {
    Literal(i32),
    Ident(Rc<String>),
    BinOp {
        op: BinOp,
        lhs: Box<ExpIR>,
        rhs: Box<ExpIR>,
    },
    UnOp {
        op: UnOp,
        val: Box<ExpIR>,
    },
}

struct RegisterAllocator {
    available_regs: Vec<RegLoc>,
    used_regs: Vec<RegLoc>,
}

impl RegisterAllocator {
    fn new() -> Self {
        RegisterAllocator {
            available_regs: vec![RegLoc::Rax, RegLoc::Rbx, RegLoc::Rcx, RegLoc::Rdx, RegLoc::Rsi, RegLoc::Rdi],
            used_regs: vec![],
        }
    }

    fn alloc_reg(&mut self) -> RegLoc {
        let reg = self.available_regs.pop().unwrap();
        self.used_regs.push(reg);
        reg
    }

    fn free_reg(&mut self, reg: RegLoc) {
        self.used_regs.retain(|r| *r != reg);
        self.available_regs.push(reg);
    }
}

//A memory allocation tracker based on RBP offset
struct MemoryAllocator {
    stack_map: HashMap<Rc<String>, i32>,
    stack_offset: i32,
}



fn eval_ir(ir: &ExprIR) -> (Vec<InsPtr>, MemoryLoc) {
    match ir {
        ExprIR::Literal(i) => MemoryLoc::Reg(RegLoc::Rax),
        ExprIR::Ident(ident) => MemoryLoc::RbpOffset(0),
        ExprIR::BinOp { op, lhs, rhs } => {
            let lhs_loc = eval_ir(lhs);
            let rhs_loc = eval_ir(rhs);
            match (lhs_loc, rhs_loc) {
                (MemoryLoc::Reg(lhs_reg), MemoryLoc::Reg(rhs_reg)) =>
            }
}
