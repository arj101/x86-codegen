use codegen::pretty_code_vec;

use crate::encode::InsPtr;
use crate::instructions::GPReg::*;
use crate::{instructions::*, quick_run};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum BinOpType {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum UnOpType {
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

impl MemoryLoc {
    fn stack_loc(&self) -> StackVal {
        match self {
            MemoryLoc::StackLoc(s) => *s,
            _ => panic!("not a stack loc"),
        }
    }
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

impl StackVal {
    fn as_rbp_offset(&self) -> i32 {
        -(self.offset as i32)
    }
}

//A memory allocation tracker based on RBP offset
struct StackAllocator {
    stack_map: HashMap<Rc<String>, StackVal>,
    alloc_gaps: Vec<StackVal>,
    stack_offset: u32,
    max_offset: u32,
}

impl StackAllocator {
    fn new() -> Self {
        StackAllocator {
            stack_map: HashMap::new(),
            stack_offset: 0,
            alloc_gaps: Vec::new(),
            max_offset: 0,
        }
    }

    fn alloc_val(&mut self, ident: Rc<String>, size: u32) -> u32 {
        let mut rbp_offset = self.stack_offset + size;

        let mut in_gap = false;
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
                in_gap = true;
            }
        }

        self.stack_map.insert(
            ident,
            StackVal {
                offset: rbp_offset,
                size,
            },
        );
        if !in_gap {
            self.stack_offset += size;
        }

        if self.stack_offset > self.max_offset {
            self.max_offset = self.stack_offset;
        }

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

impl StackIntermediateValPtr {
    fn as_rbp_offset(&self) -> i32 {
        -(self.offset as i32)
    }
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

    fn get_stack_loc(&self, name: Rc<String>) -> MemoryLoc {
        MemoryLoc::StackLoc(self.stack_alloc.get_stack_val(&name))
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
        let val = env.alloc_intermediate_val(self.0, 8);
        let offset = val.offset as i32;
        let offset = -offset;
        let code = pretty_code_vec!(
            Mov64Md32Imm32 Rbp offset self.0 ;
        );
        Ok((code, val.into()))
    }
}

struct Ident(Rc<String>);
//evaluating an identifier only retrieves the offset from the stack map. No code generation is needed
impl ExpIR for Ident {
    fn eval(&mut self, env: &mut IREnv) -> Result<(Instructions, MemoryLoc), String> {
        let loc = env.get_stack_loc(Rc::clone(&self.0));
        Ok((vec![], loc))
    }
}

struct BinOp {
    op: BinOpType,
    lhs: Box<dyn ExpIR>,
    rhs: Box<dyn ExpIR>,
}

impl ExpIR for BinOp {
    fn eval(&mut self, env: &mut IREnv) -> Result<(Instructions, MemoryLoc), String> {
        let (mut lhs_code, lhs_loc) = self.lhs.eval(env)?;
        let (mut rhs_code, rhs_loc) = self.rhs.eval(env)?;

        let mut code = vec![];

        let lhs_loc = lhs_loc.stack_loc();
        let rhs_loc = rhs_loc.stack_loc();

        code.extend(lhs_code);
        code.extend(rhs_code);

        let mut result_loc = env.alloc_intermediate_val(0, 8);

        match self.op {
            BinOpType::Add => code.extend(pretty_code_vec![
                Mov64RMd32 Rax Rbp (lhs_loc.as_rbp_offset());
                Mov64RMd32 Rbx Rbp (rhs_loc.as_rbp_offset());
                AddRR Rax Rbx;
                Mov64Md32R Rbp (result_loc.as_rbp_offset()) Rax;
            ]),
            BinOpType::Sub => code.extend(pretty_code_vec![
                Mov64RMd32 Rax Rbp (lhs_loc.as_rbp_offset());
                Mov64RMd32 Rbx Rbp (rhs_loc.as_rbp_offset());
                SubRR Rax Rbx;
                Mov64Md32R Rbp (result_loc.as_rbp_offset()) Rax;
            ]),
            BinOpType::Mul => code.extend(pretty_code_vec![
                Mov64RMd32 Rax Rbp (lhs_loc.as_rbp_offset());
                Mov64RMd32 Rbx Rbp (rhs_loc.as_rbp_offset());
                IMulR Rbx;
                Mov64Md32R Rbp (result_loc.as_rbp_offset()) Rax;
            ]),
            BinOpType::Div => code.extend(pretty_code_vec![
                Mov64RImm64 Rdx 0;
                Mov64RMd32 Rax Rbp (lhs_loc.as_rbp_offset());
                Mov64RMd32 Rbx Rbp (rhs_loc.as_rbp_offset());
                IDivR Rbx;
                Mov64Md32R Rbp (result_loc.as_rbp_offset()) Rax;
            ]),
        }

        Ok((code, result_loc.into()))
    }
}

struct UnOp {
    op: UnOpType,
    exp: Box<dyn ExpIR>,
}

impl ExpIR for UnOp {
    fn eval(&mut self, env: &mut IREnv) -> Result<(Instructions, MemoryLoc), String> {
        let (mut exp_code, exp_loc) = self.exp.eval(env)?;
        let exp_loc = exp_loc.stack_loc();
        let mut code = vec![];
        let mut result_loc = env.alloc_intermediate_val(0, 8);

        code.extend(exp_code);

        match self.op {
            UnOpType::Neg => code.extend(pretty_code_vec![
                Mov64RMd32 Rbx Rbp (exp_loc.as_rbp_offset());
                MovRImm Rax 0;
                SubRR Rax Rbx;
                Mov64Md32R Rbp (result_loc.as_rbp_offset()) Rax;
            ]),
        }

        Ok((code, result_loc.into()))
    }
}

fn eval_exp_ir(mut ir: Box<dyn ExpIR>) -> i32 {
    let mut env = IREnv::new();
    let (exp_code, result_loc) = ir.eval(&mut env).unwrap();

    let rsp_off = env.stack_alloc.max_offset;

    let mut code = pretty_code_vec![
      PushR Rbp;
      Mov64RR Rbp Rsp;
      Sub64RImm Rsp rsp_off;
    ];

    code.extend(exp_code);

    code.extend(pretty_code_vec![
        Mov64RMd32 Rax Rbp (result_loc.stack_loc().as_rbp_offset());
        Add64RImm Rsp (rsp_off.into());
        PopR Rbp;
        Ret;
    ]);

    let result: i32 = quick_run((), code);

    result
}

#[test]
fn test_ir1() {
    let mut env = IREnv::new();
    let mut exp = /*a complex expression */
        BinOp{
            op: BinOpType::Mul,
            lhs: Box::new(
                Literal(2)
            ),
            rhs: Box::new(
                BinOp{
                    op: BinOpType::Div,
                    lhs: Box::new(Literal(4)),
                    rhs: Box::new(Literal(5)),
                }
            ),
        }
        ;

    let result = eval_exp_ir(Box::new(exp));
    println!("Result = {result}");

    assert!(result == 2 * (4 / 5));
}

#[test]
fn test_ir2() {
    let mut env = IREnv::new();
    let mut exp = /*a complex expression */
        BinOp{
            op: BinOpType::Add,
            lhs: Box::new(
                BinOp{
                    op: BinOpType::Mul,
                    lhs: Box::new(Literal(2)),
                    rhs: Box::new(Literal(3)),
                }
            ),
            rhs: Box::new(
                BinOp{
                    op: BinOpType::Div,
                    lhs: Box::new(Literal(4)),
                    rhs: Box::new(Literal(5)),
                }
            ),
        }
        ;

    let result = eval_exp_ir(Box::new(exp));
    println!("Result = {result}");

    assert!(result == 2 * 3 + 4 / 5);
}

#[test]
fn test_ir3() {
    //test addition, subtraction and negation

    let mut env = IREnv::new();
    let mut exp = /*a complex expression */
        BinOp{
            op: BinOpType::Add,
            lhs: Box::new(
                BinOp{
                    op: BinOpType::Sub,
                    lhs: Box::new(Literal(2)),
                    rhs: Box::new(Literal(3)),
                }
            ),
            rhs: Box::new(
                UnOp{
                    op: UnOpType::Neg,
                    exp: Box::new(Literal(4)),
                }
            ),
        }
        ;

    let result = eval_exp_ir(Box::new(exp));
    println!("Result = {result}");

    assert!(result == 2 - 3 + -4);
}

#[test]
fn test_ir4() {
    //test highly nested expressions

    let mut env = IREnv::new();
    let mut exp = /*a complex expression */
        BinOp{
            op: BinOpType::Add,
            lhs: Box::new(
                BinOp{
                    op: BinOpType::Sub,
                    lhs: Box::new(
                        BinOp{
                            op: BinOpType::Mul,
                            lhs: Box::new(Literal(2)),
                            rhs: Box::new(Literal(3)),
                        }
                    ),
                    rhs: Box::new(
                        BinOp{
                            op: BinOpType::Div,
                            lhs: Box::new(Literal(4)),
                            rhs: Box::new(Literal(5)),
                        }
                    ),
                }
            ),
            rhs: Box::new(
                UnOp{
                    op: UnOpType::Neg,
                    exp: Box::new(
                        BinOp{
                            op: BinOpType::Add,
                            lhs: Box::new(Literal(2)),
                            rhs: Box::new(Literal(3)),
                        }
                    ),
                }
            ),
        }
        ;

    let result = eval_exp_ir(Box::new(exp));
    println!("Result = {result}");

    assert!(result == (2 * 3 - 4 / 5) + -(2 + 3));
}
