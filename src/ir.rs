use crate::instructions::*;
use std::collections::{HashMap, HashSet};
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

    While {
        body: Vec<IRIns>,
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

type VarName = Rc<String>;
type StackOffset = usize;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum VarLoc {
    RegisterLoc(GPReg),
    StackLoc(StackOffset),
}

impl VarLoc {
    fn into_reg(&self) -> Result<GPReg, ()> {
        match self {
            Self::RegisterLoc(reg) => Ok(*reg),
            _ => Err(()),
        }
    }
    fn into_stack(&self) -> Result<StackOffset, ()> {
        match self {
            Self::StackLoc(offset) => Ok(*offset),
            _ => Err(()),
        }
    }
}

#[derive(Clone)]
struct VarMetadata {
    name: VarName,
    ///this is the hone location of the variable. Every variable has a home location on the stack.
    stack_loc: StackOffset,
    ///The current location of the variable. either on stack or in a general purpose register
    var_loc: VarLoc,
}

struct IREncoder {
    encoded_ir: Vec<InsPtr>,
    var_map: HashMap<VarName, VarMetadata>,
    stack_rev_map: HashMap<StackOffset, VarName>,
    reg_rev_map: HashMap<GPReg, VarName>,
    available_regs: HashSet<GPReg>,
    free_regs: HashSet<GPReg>,
    stack_top: usize,
    auto_name_count: usize,

    temp_scopes: Vec<HashSet<VarName>>,
}

impl Default for IREncoder {
    fn default() -> Self {
        IREncoder {
            encoded_ir: vec![],
            var_map: HashMap::new(),
            stack_rev_map: HashMap::new(),
            reg_rev_map: HashMap::new(),
            stack_top: 0,
            auto_name_count: 0,
            free_regs: [Rax, Rcx, Rdx, Rsi, Rdi].into(),
            available_regs: [Rax, Rcx, Rdx, Rsi, Rdi].into(),
            temp_scopes: vec![HashSet::new()],
        }
    }
}

impl IREncoder {
    fn unfree_reg(&mut self, reg: GPReg) -> Result<(), ()> {
        if self.free_regs.get(&reg).is_some() {
            self.free_regs.remove(&reg);
            return Ok(());
        }
        Err(())
    }

    fn free_reg(&mut self, reg: GPReg) -> Result<(), ()> {
        if self.reg_rev_map.get(&reg).is_some() {
            self.reg_rev_map.remove(&reg);
            self.free_regs.insert(reg);
            return Ok(());
        }
        Err(())
    }

    fn unload_reg(&mut self, reg: GPReg) {
        if let Some(varname) = self.reg_rev_map.get(&reg) {
            self.move_var_to_stack(&varname.clone(), true);
        }
    }

    fn unfree_any_reg(&mut self) -> Result<GPReg, ()> {
        if self.free_regs.len() == 0 {
            return Err(());
        }
        let reg = self.free_regs.iter().next().unwrap().clone();
        self.unfree_reg(reg);
        Ok(reg)
    }

    fn unfree_or_move_any_reg(&mut self, in_use: &[GPReg]) -> GPReg {
        if self.free_regs.len() == 0 {
            //free the first usable register
            let reg = self
                .available_regs
                .iter()
                .filter(|r| !in_use.contains(r))
                .next()
                .unwrap()
                .clone();
            self.move_var_to_stack(&self.reg_rev_map.get(&reg).unwrap().clone(), true);
        }
        self.unfree_any_reg().expect("Unfree should succeed")
    }

    fn alloc_var(&mut self, varname: VarName, size: usize) {
        assert!(size == 8, "variable size 8 bytes only supported");
        assert!(
            size % 4 == 0,
            "Variable size should be multiples of 4 bytes"
        );

        let loc = self.stack_top;
        let var = VarMetadata {
            name: Rc::clone(&varname),
            stack_loc: loc,
            var_loc: VarLoc::StackLoc(loc),
        };

        self.stack_top += size;
        self.var_map.insert(Rc::clone(&varname), var);
        self.stack_rev_map.insert(loc, varname);
    }

    fn move_var_to_stack(&mut self, varname: &VarName, gen_code: bool) {
        println!("Moving variable to stack: {}", varname);
        let var = self
            .var_map
            .get(varname)
            .map(|v| v.clone())
            .expect("Unknown variable");
        match var.var_loc {
            VarLoc::StackLoc(_) => println!("Variable already on stack, not moving"),
            VarLoc::RegisterLoc(reg) => {
                assert!(
                    **self.reg_rev_map.get(&reg).unwrap() == **varname,
                    "unexpected reg var map"
                );
                assert!(self.free_reg(reg).is_ok());
                assert!(
                    self.stack_rev_map.get(&var.stack_loc).is_none(),
                    "variable home stack loc was reallocated by another"
                );
                self.stack_rev_map.insert(var.stack_loc, Rc::clone(varname));

                self.var_map.get_mut(varname).unwrap().var_loc = VarLoc::StackLoc(var.stack_loc);

                if gen_code {
                    let ins = pretty_code_vec![
                        Mov64Md32R Rbp (-(var.stack_loc as i32 +8)) reg;
                    ];
                    self.encoded_ir.extend(ins);
                }
            }
        }
    }

    fn assign_var_value(&mut self, varname: &VarName, value: i32) {
        let var = self.var_map.get(varname).expect("Did not find variable");
        match var.var_loc {
            VarLoc::RegisterLoc(reg) => {
                self.encoded_ir.extend(pretty_code_vec![
                    MovRImm reg value;
                ]);
            }
            VarLoc::StackLoc(stack_off) => {
                self.encoded_ir.extend(pretty_code_vec![
                    Mov64Md32Imm32 Rbp (-(stack_off as i32 + 8)) value;
                ]);
            }
        }
    }

    fn move_var_to_reg(&mut self, varname: &VarName, new_reg: GPReg, gen_code: bool) {
        let var = self.var_map.get(varname).expect("Unknown variable").clone();
        println!("Moving variable {:?} to register {:?}", varname, new_reg);
        match var.var_loc {
            VarLoc::RegisterLoc(reg) if reg == new_reg => {
                println!("Variable already in the same register, not moving")
            }
            VarLoc::RegisterLoc(reg) => {
                assert!(
                    **self.reg_rev_map.get(&reg).unwrap() == **varname,
                    "unexpected reg var map"
                );
                self.reg_rev_map.remove(&reg);
                assert!(
                    self.reg_rev_map.get(&new_reg).is_none(),
                    "register already occupied"
                );
                assert!(self.unfree_reg(new_reg).is_ok());

                self.reg_rev_map.insert(new_reg, Rc::clone(varname));
                self.var_map.get_mut(varname).unwrap().var_loc = VarLoc::RegisterLoc(new_reg);
                if gen_code {
                    let ins = pretty_code_vec![
                        Mov64RR new_reg reg;
                    ];
                    self.encoded_ir.extend(ins);
                }
            }
            VarLoc::StackLoc(stack_off) => {
                assert!(
                    **self.stack_rev_map.get(&stack_off).unwrap() == **varname,
                    "unexpected stack var map"
                );
                self.stack_rev_map.remove(&stack_off);
                assert!(
                    self.reg_rev_map.get(&new_reg).is_none(),
                    "register already occupied"
                );
                self.unfree_reg(new_reg);
                self.reg_rev_map.insert(new_reg, Rc::clone(varname));
                self.var_map.get_mut(varname).unwrap().var_loc = VarLoc::RegisterLoc(new_reg);
                if gen_code {
                    let ins = pretty_code_vec![
                        Mov64RMd32 new_reg Rbp (-(stack_off as i32 + 8));
                    ];
                    self.encoded_ir.extend(ins);
                }
            }
        }
    }

    ///loads variable onto a register, usually for performing an operation, but first unloads the
    ///current value on the register, if there are any, to the stack
    fn load_var(&mut self, varname: &VarName, reg: GPReg) {
        if let Some(other_var) = self.reg_rev_map.get(&reg).map(|v| v.clone()) {
            self.move_var_to_stack(&other_var, true);
        }
        self.move_var_to_reg(varname, reg, true);
    }

    ///creates a temporary copy of a variable on a register
    ///does this by moving the variable in ad out of the specified register to leave a copy in
    ///place. The copy is not tracked and will be overwritten by other allocations
    fn load_var_temp_copy_to_reg(&mut self, varname: &VarName, reg: GPReg) {
        let var = self.var_map.get(varname).expect("variable not found");
        let original_loc = var.var_loc;

        self.move_var_to_reg(varname, reg, true);
        match original_loc {
            VarLoc::StackLoc(_) => {
                self.move_var_to_stack(varname, false);
            }
            VarLoc::RegisterLoc(prev_reg) => {
                self.move_var_to_reg(varname, prev_reg, false);
            }
        }
    }

    fn push_tempscope(&mut self) {
        self.temp_scopes.push(HashSet::new());
    }

    fn pop_tempscope(&mut self) {
        assert!(self.temp_scopes.len() > 0, "no tempscope to pop");
    }

    fn free_tempvars(&mut self) {
        let temp_scope = self.temp_scopes.last().unwrap();
        for varname in temp_scope.clone() {
            let var = self.var_map.get(&varname).unwrap();
            if let VarLoc::RegisterLoc(reg) = var.var_loc {
                self.free_reg(reg);
            }
        }
    }

    fn alloc_regvar(&mut self, name: VarName) -> Result<GPReg, ()> {
        let reg = self.unfree_any_reg()?;
        let var_meta = VarMetadata {
            name: Rc::clone(&name),
            var_loc: VarLoc::RegisterLoc(reg),
            stack_loc: 0,
        };
        self.var_map.insert(Rc::clone(&name), var_meta);
        self.reg_rev_map.insert(reg, name);

        Ok(reg)
    }

    fn alloc_tempvar(&mut self, prefer_register: bool) -> VarName {
        let name = format!("temp_{}", self.auto_name_count);
        self.auto_name_count += 1;
        self.temp_scopes
            .last_mut()
            .unwrap()
            .insert(Rc::new(name.clone()));
        let varname = Rc::new(name);

        if let Ok(reg) = self.alloc_regvar(Rc::clone(&varname)) {
            println!("Allocated tempvar {:?} to register {:?}", varname, reg);
            return varname;
        }

        self.alloc_var(Rc::clone(&varname), 8);
        println!("Allocated tempvar {:?} to stack", varname);
        varname
    }

    fn load_or_alloc_val(&mut self, var: &Val<i32>, prefer_register: bool) -> VarName {
        match var {
            Val::Ident(varname) => Rc::clone(varname),
            Val::Literal(val) => {
                let varname = self.alloc_tempvar(prefer_register);
                self.assign_var_value(&varname, *val);
                varname
            }
        }
    }

    fn move_var_to_any_reg(&mut self, var: &VarName, in_use: &[GPReg]) -> GPReg {
        match self.var_map.get(var).unwrap().var_loc {
            VarLoc::RegisterLoc(reg) => reg,
            VarLoc::StackLoc(_) => {
                let reg = self.unfree_or_move_any_reg(in_use);
                self.move_var_to_reg(var, reg, true);
                reg
            }
        }
    }

    fn encode_ins(&mut self, ir: IRIns) {
        self.push_tempscope();
        match ir {
            IRIns::InitStore32 { dest, val } => {
                self.alloc_var(Rc::clone(&dest), 8);
                self.assign_var_value(&dest, val);
            }
            IRIns::Store32 { dest, val } => {
                self.assign_var_value(&dest, val);
            }
            IRIns::Add32 { dest, val1, val2 } => {
                let var1 = self.load_or_alloc_val(&val1, true);
                let var2 = self.load_or_alloc_val(&val2, true);

                if **var1 == **dest || **var2 == **dest {
                    let var2 = if **var1 == **dest { var2 } else { var1 };
                    let rsrc = self.move_var_to_any_reg(&var2, &[]);
                    let rdest = self.move_var_to_any_reg(&dest, &[rsrc]);

                    self.encoded_ir.extend(pretty_code_vec![
                        AddRR rdest rsrc;
                    ]);
                } else {
                    let dest_reg = self.unfree_or_move_any_reg(&[]);
                    self.load_var_temp_copy_to_reg(&var1, dest_reg);

                    self.move_var_to_reg(&dest, dest_reg, false);
                    let src_reg = self.move_var_to_any_reg(&var2, &[dest_reg]);

                    self.encoded_ir.extend(pretty_code_vec![
                        AddRR dest_reg src_reg;
                    ]);
                }
            }

            IRIns::Sub32 { dest, val1, val2 } => {
                let var1 = self.load_or_alloc_val(&val1, true);
                let var2 = self.load_or_alloc_val(&val2, true);

                if **var1 == **dest {
                    let rsrc = self.move_var_to_any_reg(&var2, &[]);
                    let rdest = self.move_var_to_any_reg(&dest, &[rsrc]);

                    self.encoded_ir.extend(pretty_code_vec![
                        SubRR rdest rsrc;
                    ]);
                } else {
                    let dest_reg = self.unfree_or_move_any_reg(&[]);
                    self.load_var_temp_copy_to_reg(&var1, dest_reg);

                    self.move_var_to_reg(&dest, dest_reg, false);
                    let src_reg = self.move_var_to_any_reg(&var2, &[dest_reg]);

                    self.encoded_ir.extend(pretty_code_vec![
                        SubRR dest_reg src_reg;
                    ]);
                }
            }

            IRIns::Mul32 { dest, val1, val2 } => {
                self.unload_reg(Rax);
                self.unload_reg(Rdx);

                self.unfree_reg(Rax);
                self.unfree_reg(Rdx);
                let var1 = self.load_or_alloc_val(&val1, true);
                let var2 = self.load_or_alloc_val(&val2, true);
                self.free_reg(Rax);
                self.free_reg(Rdx);

                if **var1 == **dest || **var2 == **dest {
                    let var2 = if **var1 == **dest { var2 } else { var1 };
                    let rdst = self.move_var_to_reg(&dest, Rax, true);
                    let src = self.move_var_to_any_reg(&var2, &[Rax, Rdx]);

                    self.encoded_ir.extend(pretty_code_vec![
                        IMulR src;
                    ])
                } else {
                    self.load_var_temp_copy_to_reg(&var1, Rax);
                    self.move_var_to_reg(&dest, Rax, false);

                    let src = self.move_var_to_any_reg(&var2, &[Rax, Rdx]);

                    self.encoded_ir.extend(pretty_code_vec![
                        IMulR src;
                    ])
                }
            }

            IRIns::Print32 { val } => {
                // self.unload_reg(Rdi); //function parameter
                // self.unload_reg(Rcx); //return value
                let caller_saved = [Rax, Rcx, Rdx, Rsi, Rdi];
                for reg in caller_saved {
                    self.unload_reg(reg);
                }

                let var1 = self.load_or_alloc_val(&val, true);
                self.load_var_temp_copy_to_reg(&var1, Rdi);

                self.encoded_ir.extend(pretty_code_vec![
                    CallM Rbx;
                ])
            }
            _ => todo!(),
        }
        self.free_tempvars();
        self.pop_tempscope();
    }
}

pub fn ir_encode_fn(inss: Vec<IRIns>) -> Vec<InsPtr> {
    let mut encoded = vec![];
    // let mut stack = StackMap::new();
    let mut encoder = IREncoder::default();

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
        encoder.encode_ins(ins);
        // use IRIns::*;
        // match ins {
        //     InitStore32 { dest, val } => {
        //         stack.insert(Rc::clone(&dest), 8);
        //         let StackLoc { rbp_offset, .. } = stack.get(&dest);
        //         let off = 0 - rbp_offset as i32;
        //
        //         encoded.extend(pretty_code_vec![
        //             Mov64Md32Imm32 Rbp off val;
        //         ]);
        //     }
        //     Add32 { dest, val1, val2 } => {
        //         let StackLoc { rbp_offset, .. } = stack.get(&dest);
        //         let off = 0 - rbp_offset as i32;
        //
        //         match (val1, val2) {
        //             (Val::Ident(v1), Val::Literal(l2)) => {
        //                 let StackLoc {
        //                     rbp_offset: v1_off, ..
        //                 } = stack.get(&dest);
        //                 let off_1 = 0 - v1_off as i32;
        //                 encoded.extend(pretty_code_vec![
        //                     Mov64RMd32 Rdi Rbp off_1;
        //                     Mov64RImm64 Rsi (l2.into());
        //                     AddRR Rdi Rsi;
        //                     Mov64Md32R Rbp off_1 Rdi;
        //                 ]);
        //             }
        //             _ => unimplemented!(),
        //         }
        //     }
        //     Print32 { val } => match val {
        //         Val::Literal(v) => {
        //             let v = v.into();
        //             encoded.extend(pretty_code_vec![
        //                 Mov64RImm64 Rdi v;
        //                 CallM Rbx;
        //             ]);
        //         }
        //         Val::Ident(dest) => {
        //             let StackLoc { rbp_offset, .. } = stack.get(&dest);
        //
        //             let off = 0 - rbp_offset as i32;
        //             let rsp = 16 as i32;
        //             encoded.extend(pretty_code_vec![
        //                 Mov64RMd32 Rdi Rbp off;
        //                     // Mov64RR Rdi Rdx;
        //                 Mov64RR Rax Rbx;
        //                 // PushR Rdi;
        //
        //                 // Mov64RR Rdi Rbp;
        //             //  Pus
        //                 // PushR Rax;
        //                 // Sub64RImm Rsp 0x28;
        //                 // Mov64RR Rbx Rbx;
        //             // CallR Rax;
        //                 CallM Rax;
        //                 // Add64RImm Rsp 0x28;
        //                 // PopR Rax;
        //
        //                 // PopR Rdi;
        //
        //                 // Mov64RR Rdi Rbp;
        //                 // CallM Rsi;
        //             ]);
        //         }
        //     },
        //     _ => todo!(),
        // }
    }
    encoded.extend(encoder.encoded_ir);

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

//generate test cases for ir_encode_fn from example in main.rs
#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use super::*;

    #[test]
    fn test_ir_encode() {
        let mut f = |i: i32| {
            println!("value = {i}");
        };

        let fn_mut_ref = &mut f;
        let fc_ = libffi::high::Closure1::new(&mut f);
        let fc = fc_.code_ptr();
        let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

        macro_rules! ident {
            ($s:expr) => {
                Rc::new(($s).to_string())
            };
        };

        let inss = ir_encode_fn(vec![
            IRIns::InitStore32 {
                dest: ident!("hello"),
                val: 123,
            },
            IRIns::InitStore32 {
                dest: ident!("hello1"),
                val: 10,
            },
            // IRIns::Print32 {
            //     val: Val::Literal(12),
            // },
            // IRIns::Print32 {
            //     val: Val::Ident(Rc::new("hello".to_string())),
            // },
            IRIns::Print32 {
                val: Val::Ident(Rc::new("hello".to_string())),
            },
            IRIns::Add32 {
                dest: ident!("hello"),
                val1: Val::Ident(ident!("hello")),
                val2: Val::Literal(-23),
            },
            IRIns::Print32 {
                val: Val::Ident(Rc::new("hello".to_string())),
            },
            IRIns::Sub32 {
                dest: ident!("hello"),
                val1: Val::Ident(ident!("hello")),
                val2: Val::Literal(23),
            },
            IRIns::Print32 {
                val: Val::Ident(Rc::new("hello".to_string())),
            },
            IRIns::Add32 {
                dest: ident!("hello"),
                val1: Val::Ident(ident!("hello1")),
                val2: Val::Literal(-32),
            },
            IRIns::Print32 {
                val: Val::Ident(Rc::new("hello".to_string())),
            },
        ]);

        let f = encode!(no_disasm, inss => extern "C" fn(extern "C" fn (i32) -> ()) -> i32);
        let result = f(fc_ptr);
        println!("result = {result:#X}");
    }
    #[test]
    fn test_ir_complex_operations() {
        let mut output = Vec::new();
        let oref = &mut output;
        let mut f = |i: i32| {
            oref.push(i);
        };

        let fc_ = libffi::high::ClosureMut1::new(&mut f);
        let fc = fc_.code_ptr();
        let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

        macro_rules! ident {
            ($s:expr) => {
                Rc::new(($s).to_string())
            };
        };

        let inss = ir_encode_fn(vec![
            IRIns::InitStore32 {
                dest: ident!("var1"),
                val: 50,
            },
            IRIns::InitStore32 {
                dest: ident!("var2"),
                val: 20,
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            }, // Expected output: 50
            IRIns::Add32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Literal(10),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            }, // Expected output: 60
            IRIns::Sub32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Ident(ident!("var2")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            }, // Expected output: 40
            IRIns::Add32 {
                dest: ident!("var2"),
                val1: Val::Ident(ident!("var2")),
                val2: Val::Literal(30),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var2")),
            }, // Expected output: 50
        ]);

        let f = encode!(no_disasm, inss => extern "C" fn(extern "C" fn(i32) -> ()) -> i32);
        f(fc_ptr);

        drop(fc_);

        // Validate the output
        assert_eq!(
            output,
            vec![50, 60, 40, 50],
            "Unexpected output for complex operations"
        );
    }

    #[test]
    fn test_ir_edge_cases() {
        let mut output = Vec::new();
        let oref = &mut output;
        let mut f = |i: i32| {
            oref.push(i);
        };

        let fc_ = libffi::high::ClosureMut1::new(&mut f);
        let fc = fc_.code_ptr();
        let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

        macro_rules! ident {
            ($s:expr) => {
                Rc::new(($s).to_string())
            };
        };

        let inss = ir_encode_fn(vec![
            IRIns::InitStore32 {
                dest: ident!("var1"),
                val: 0,
            },
            IRIns::InitStore32 {
                dest: ident!("var2"),
                val: -10,
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            }, // Expected output: 0
            IRIns::Print32 {
                val: Val::Ident(ident!("var2")),
            }, // Expected output: -10
            IRIns::Add32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Ident(ident!("var2")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            }, // Expected output: -10
            IRIns::Sub32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Literal(-20),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            }, // Expected output: 10
        ]);

        let f = encode!(no_disasm, inss => extern "C" fn(extern "C" fn(i32) -> ()) -> i32);
        f(fc_ptr);

        drop(fc_);

        // Validate the output
        assert_eq!(
            output,
            vec![0, -10, -10, 10],
            "Unexpected output for edge cases"
        );
    }
    #[test]
    fn test_ir_large_numbers() {
        let mut output = Vec::new();
        let oref = &mut output;
        let mut f = |i: i32| {
            oref.push(i);
        };

        let fc_ = libffi::high::ClosureMut1::new(&mut f);
        let fc = fc_.code_ptr();
        let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

        macro_rules! ident {
            ($s:expr) => {
                Rc::new(($s).to_string())
            };
        };

        let inss = ir_encode_fn(vec![
            IRIns::InitStore32 {
                dest: ident!("large1"),
                val: i32::MAX,
            },
            IRIns::InitStore32 {
                dest: ident!("large2"),
                val: i32::MIN,
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("large1")),
            }, // Expected output: i32::MAX
            IRIns::Print32 {
                val: Val::Ident(ident!("large2")),
            }, // Expected output: i32::MIN
            IRIns::Add32 {
                dest: ident!("large1"),
                val1: Val::Ident(ident!("large1")),
                val2: Val::Literal(-1),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("large1")),
            }, // Expected output: i32::MAX - 1
            IRIns::Add32 {
                dest: ident!("large2"),
                val1: Val::Literal(i32::MAX),
                val2: Val::Literal(2),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("large2")),
            }, // Expected output: i32::MIN + 1
        ]);

        let f = encode!(no_disasm, inss => extern "C" fn(extern "C" fn(i32) -> ()) -> i32);
        f(fc_ptr);

        drop(fc_);

        // Validate the output
        assert_eq!(
            output,
            vec![i32::MAX, i32::MIN, i32::MAX - 1, i32::MIN + 1],
            "Unexpected output for large numbers"
        );
    }

    #[test]
    fn test_ir_chained_operations() {
        let mut output = Vec::new();
        let oref = &mut output;
        let mut f = |i: i32| {
            oref.push(i);
        };

        let fc_ = libffi::high::ClosureMut1::new(&mut f);
        let fc = fc_.code_ptr();
        let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

        macro_rules! ident {
            ($s:expr) => {
                Rc::new(($s).to_string())
            };
        };

        let inss = ir_encode_fn(vec![
            IRIns::InitStore32 {
                dest: ident!("var1"),
                val: 5,
            },
            IRIns::Add32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Literal(3),
            },
            IRIns::Sub32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Literal(2),
            },
            IRIns::Add32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Literal(10),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            }, // Expected output: 16
        ]);

        let f = encode!(no_disasm, inss => extern "C" fn(extern "C" fn(i32) -> ()) -> i32);
        f(fc_ptr);

        drop(fc_);

        // Validate the output
        assert_eq!(output, vec![16], "Unexpected output for chained operations");
    }
    #[test]
    fn test_ir_multiple_variables() {
        let mut output = Vec::new();
        let oref = &mut output;
        let mut f = |i: i32| {
            oref.push(i);
        };

        let fc_ = libffi::high::ClosureMut1::new(&mut f);
        let fc = fc_.code_ptr();
        let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

        macro_rules! ident {
            ($s:expr) => {
                Rc::new(($s).to_string())
            };
        };

        let inss = ir_encode_fn(vec![
            IRIns::InitStore32 {
                dest: ident!("var1"),
                val: 15,
            },
            IRIns::InitStore32 {
                dest: ident!("var2"),
                val: 10,
            },
            IRIns::InitStore32 {
                dest: ident!("var3"),
                val: 0,
            },
            IRIns::InitStore32 {
                dest: ident!("var4"),
                val: 0,
            },
            IRIns::Add32 {
                dest: ident!("var3"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Ident(ident!("var2")),
            },
            IRIns::Sub32 {
                dest: ident!("var4"),
                val1: Val::Ident(ident!("var3")),
                val2: Val::Literal(5),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var4")),
            }, // Expected output: 20
        ]);

        let f = encode!(no_disasm, inss => extern "C" fn(extern "C" fn(i32) -> ()) -> i32);
        f(fc_ptr);

        drop(fc_);

        // Validate the output
        assert_eq!(output, vec![20], "Unexpected output for multiple variables");
    }
    #[test]
    fn test_ir_40_variables() {
        let mut output = Vec::new();
        let oref = &mut output;
        let mut f = |i: i32| {
            oref.push(i);
        };

        let fc_ = libffi::high::ClosureMut1::new(&mut f);
        let fc = fc_.code_ptr();
        let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

        macro_rules! ident {
            ($s:expr) => {
                Rc::new(($s).to_string())
            };
        };

        let inss = ir_encode_fn(vec![
            // Initializing 40 variables
            IRIns::InitStore32 {
                dest: ident!("var1"),
                val: 1,
            },
            IRIns::InitStore32 {
                dest: ident!("var2"),
                val: 2,
            },
            IRIns::InitStore32 {
                dest: ident!("var3"),
                val: 3,
            },
            IRIns::InitStore32 {
                dest: ident!("var4"),
                val: 4,
            },
            IRIns::InitStore32 {
                dest: ident!("var5"),
                val: 5,
            },
            IRIns::InitStore32 {
                dest: ident!("var6"),
                val: 6,
            },
            IRIns::InitStore32 {
                dest: ident!("var7"),
                val: 7,
            },
            IRIns::InitStore32 {
                dest: ident!("var8"),
                val: 8,
            },
            IRIns::InitStore32 {
                dest: ident!("var9"),
                val: 9,
            },
            IRIns::InitStore32 {
                dest: ident!("var10"),
                val: 10,
            },
            IRIns::InitStore32 {
                dest: ident!("var11"),
                val: 11,
            },
            IRIns::InitStore32 {
                dest: ident!("var12"),
                val: 12,
            },
            IRIns::InitStore32 {
                dest: ident!("var13"),
                val: 13,
            },
            IRIns::InitStore32 {
                dest: ident!("var14"),
                val: 14,
            },
            IRIns::InitStore32 {
                dest: ident!("var15"),
                val: 15,
            },
            IRIns::InitStore32 {
                dest: ident!("var16"),
                val: 16,
            },
            IRIns::InitStore32 {
                dest: ident!("var17"),
                val: 17,
            },
            IRIns::InitStore32 {
                dest: ident!("var18"),
                val: 18,
            },
            IRIns::InitStore32 {
                dest: ident!("var19"),
                val: 19,
            },
            IRIns::InitStore32 {
                dest: ident!("var20"),
                val: 20,
            },
            IRIns::InitStore32 {
                dest: ident!("var21"),
                val: 21,
            },
            IRIns::InitStore32 {
                dest: ident!("var22"),
                val: 22,
            },
            IRIns::InitStore32 {
                dest: ident!("var23"),
                val: 23,
            },
            IRIns::InitStore32 {
                dest: ident!("var24"),
                val: 24,
            },
            IRIns::InitStore32 {
                dest: ident!("var25"),
                val: 25,
            },
            IRIns::InitStore32 {
                dest: ident!("var26"),
                val: 26,
            },
            IRIns::InitStore32 {
                dest: ident!("var27"),
                val: 27,
            },
            IRIns::InitStore32 {
                dest: ident!("var28"),
                val: 28,
            },
            IRIns::InitStore32 {
                dest: ident!("var29"),
                val: 29,
            },
            IRIns::InitStore32 {
                dest: ident!("var30"),
                val: 30,
            },
            IRIns::InitStore32 {
                dest: ident!("var31"),
                val: 31,
            },
            IRIns::InitStore32 {
                dest: ident!("var32"),
                val: 32,
            },
            IRIns::InitStore32 {
                dest: ident!("var33"),
                val: 33,
            },
            IRIns::InitStore32 {
                dest: ident!("var34"),
                val: 34,
            },
            IRIns::InitStore32 {
                dest: ident!("var35"),
                val: 35,
            },
            IRIns::InitStore32 {
                dest: ident!("var36"),
                val: 36,
            },
            IRIns::InitStore32 {
                dest: ident!("var37"),
                val: 37,
            },
            IRIns::InitStore32 {
                dest: ident!("var38"),
                val: 38,
            },
            IRIns::InitStore32 {
                dest: ident!("var39"),
                val: 39,
            },
            IRIns::InitStore32 {
                dest: ident!("var40"),
                val: 40,
            },
            // Performing alternating operations on the variables
            IRIns::Add32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var2")),
                val2: Val::Ident(ident!("var3")),
            },
            IRIns::Sub32 {
                dest: ident!("var4"),
                val1: Val::Ident(ident!("var5")),
                val2: Val::Ident(ident!("var6")),
            },
            IRIns::Add32 {
                dest: ident!("var7"),
                val1: Val::Ident(ident!("var8")),
                val2: Val::Ident(ident!("var9")),
            },
            IRIns::Sub32 {
                dest: ident!("var10"),
                val1: Val::Ident(ident!("var11")),
                val2: Val::Ident(ident!("var12")),
            },
            IRIns::Add32 {
                dest: ident!("var13"),
                val1: Val::Ident(ident!("var14")),
                val2: Val::Ident(ident!("var15")),
            },
            IRIns::Sub32 {
                dest: ident!("var16"),
                val1: Val::Ident(ident!("var17")),
                val2: Val::Ident(ident!("var18")),
            },
            IRIns::Add32 {
                dest: ident!("var19"),
                val1: Val::Ident(ident!("var20")),
                val2: Val::Ident(ident!("var21")),
            },
            IRIns::Sub32 {
                dest: ident!("var22"),
                val1: Val::Ident(ident!("var23")),
                val2: Val::Ident(ident!("var24")),
            },
            IRIns::Add32 {
                dest: ident!("var25"),
                val1: Val::Ident(ident!("var26")),
                val2: Val::Ident(ident!("var27")),
            },
            IRIns::Sub32 {
                dest: ident!("var28"),
                val1: Val::Ident(ident!("var29")),
                val2: Val::Ident(ident!("var30")),
            },
            IRIns::Add32 {
                dest: ident!("var31"),
                val1: Val::Ident(ident!("var32")),
                val2: Val::Ident(ident!("var33")),
            },
            IRIns::Sub32 {
                dest: ident!("var34"),
                val1: Val::Ident(ident!("var35")),
                val2: Val::Ident(ident!("var36")),
            },
            IRIns::Add32 {
                dest: ident!("var37"),
                val1: Val::Ident(ident!("var38")),
                val2: Val::Ident(ident!("var39")),
            },
            IRIns::Sub32 {
                dest: ident!("var40"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Ident(ident!("var2")),
            },
            // Repeating operations with different combinations for variables
            IRIns::Add32 {
                dest: ident!("var3"),
                val1: Val::Ident(ident!("var4")),
                val2: Val::Ident(ident!("var5")),
            },
            IRIns::Sub32 {
                dest: ident!("var6"),
                val1: Val::Ident(ident!("var7")),
                val2: Val::Ident(ident!("var8")),
            },
            IRIns::Add32 {
                dest: ident!("var9"),
                val1: Val::Ident(ident!("var10")),
                val2: Val::Ident(ident!("var11")),
            },
            IRIns::Sub32 {
                dest: ident!("var12"),
                val1: Val::Ident(ident!("var13")),
                val2: Val::Ident(ident!("var14")),
            },
            IRIns::Add32 {
                dest: ident!("var15"),
                val1: Val::Ident(ident!("var16")),
                val2: Val::Ident(ident!("var17")),
            },
            IRIns::Sub32 {
                dest: ident!("var18"),
                val1: Val::Ident(ident!("var19")),
                val2: Val::Ident(ident!("var20")),
            },
            IRIns::Add32 {
                dest: ident!("var21"),
                val1: Val::Ident(ident!("var22")),
                val2: Val::Ident(ident!("var23")),
            },
            IRIns::Sub32 {
                dest: ident!("var24"),
                val1: Val::Ident(ident!("var25")),
                val2: Val::Ident(ident!("var26")),
            },
            IRIns::Add32 {
                dest: ident!("var27"),
                val1: Val::Ident(ident!("var28")),
                val2: Val::Ident(ident!("var29")),
            },
            IRIns::Sub32 {
                dest: ident!("var30"),
                val1: Val::Ident(ident!("var31")),
                val2: Val::Ident(ident!("var32")),
            },
            IRIns::Add32 {
                dest: ident!("var33"),
                val1: Val::Ident(ident!("var34")),
                val2: Val::Ident(ident!("var35")),
            },
            IRIns::Sub32 {
                dest: ident!("var36"),
                val1: Val::Ident(ident!("var37")),
                val2: Val::Ident(ident!("var38")),
            },
            IRIns::Add32 {
                dest: ident!("var39"),
                val1: Val::Ident(ident!("var40")),
                val2: Val::Ident(ident!("var1")),
            },
            // Final print to check final values
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var2")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var3")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var4")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var5")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var6")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var7")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var8")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var9")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var10")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var11")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var12")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var13")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var14")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var15")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var16")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var17")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var18")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var19")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var20")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var21")),
            },
        ]);

        let f = encode!(no_disasm,inss => extern "C" fn(extern "C" fn(i32) -> ()) -> i32);
        f(fc_ptr);

        drop(fc_);

        // unfortunately chatgpt gave up on generating the output validation and im too lazy to do
        // it myself. So this is now an allocation failure test
    }
    #[test]
    fn test_ir_20_variables() {
        let mut output = Vec::new();
        let oref = &mut output;
        let mut f = |i: i32| {
            oref.push(i);
        };

        let fc_ = libffi::high::ClosureMut1::new(&mut f);
        let fc = fc_.code_ptr();
        let fc_ptr: extern "C" fn(i32) -> () = unsafe { std::mem::transmute(fc) };

        macro_rules! ident {
            ($s:expr) => {
                Rc::new(($s).to_string())
            };
        };

        let inss = ir_encode_fn(vec![
            // Initializing 20 variables
            IRIns::InitStore32 {
                dest: ident!("var1"),
                val: 1,
            },
            IRIns::InitStore32 {
                dest: ident!("var2"),
                val: 2,
            },
            IRIns::InitStore32 {
                dest: ident!("var3"),
                val: 3,
            },
            IRIns::InitStore32 {
                dest: ident!("var4"),
                val: 4,
            },
            IRIns::InitStore32 {
                dest: ident!("var5"),
                val: 5,
            },
            IRIns::InitStore32 {
                dest: ident!("var6"),
                val: 6,
            },
            IRIns::InitStore32 {
                dest: ident!("var7"),
                val: 7,
            },
            IRIns::InitStore32 {
                dest: ident!("var8"),
                val: 8,
            },
            IRIns::InitStore32 {
                dest: ident!("var9"),
                val: 9,
            },
            IRIns::InitStore32 {
                dest: ident!("var10"),
                val: 10,
            },
            IRIns::InitStore32 {
                dest: ident!("var11"),
                val: 11,
            },
            IRIns::InitStore32 {
                dest: ident!("var12"),
                val: 12,
            },
            IRIns::InitStore32 {
                dest: ident!("var13"),
                val: 13,
            },
            IRIns::InitStore32 {
                dest: ident!("var14"),
                val: 14,
            },
            IRIns::InitStore32 {
                dest: ident!("var15"),
                val: 15,
            },
            IRIns::InitStore32 {
                dest: ident!("var16"),
                val: 16,
            },
            IRIns::InitStore32 {
                dest: ident!("var17"),
                val: 17,
            },
            IRIns::InitStore32 {
                dest: ident!("var18"),
                val: 18,
            },
            IRIns::InitStore32 {
                dest: ident!("var19"),
                val: 19,
            },
            IRIns::InitStore32 {
                dest: ident!("var20"),
                val: 20,
            },
            // Performing alternating operations on the variables
            IRIns::Add32 {
                dest: ident!("var1"),
                val1: Val::Ident(ident!("var2")),
                val2: Val::Ident(ident!("var3")),
            }, // var1 = var2 + var3
            IRIns::Sub32 {
                dest: ident!("var4"),
                val1: Val::Ident(ident!("var5")),
                val2: Val::Ident(ident!("var6")),
            }, // var4 = var5 - var6
            IRIns::Add32 {
                dest: ident!("var7"),
                val1: Val::Ident(ident!("var8")),
                val2: Val::Ident(ident!("var9")),
            }, // var7 = var8 + var9
            IRIns::Sub32 {
                dest: ident!("var10"),
                val1: Val::Ident(ident!("var11")),
                val2: Val::Ident(ident!("var12")),
            }, // var10 = var11 - var12
            IRIns::Add32 {
                dest: ident!("var13"),
                val1: Val::Ident(ident!("var14")),
                val2: Val::Ident(ident!("var15")),
            }, // var13 = var14 + var15
            IRIns::Sub32 {
                dest: ident!("var16"),
                val1: Val::Ident(ident!("var17")),
                val2: Val::Ident(ident!("var18")),
            }, // var16 = var17 - var18
            IRIns::Add32 {
                dest: ident!("var19"),
                val1: Val::Ident(ident!("var20")),
                val2: Val::Ident(ident!("var1")),
            }, // var19 = var20 + var1
            IRIns::Sub32 {
                dest: ident!("var2"),
                val1: Val::Ident(ident!("var3")),
                val2: Val::Ident(ident!("var4")),
            }, // var2 = var3 - var4
            IRIns::Add32 {
                dest: ident!("var5"),
                val1: Val::Ident(ident!("var6")),
                val2: Val::Ident(ident!("var7")),
            }, // var5 = var6 + var7
            IRIns::Sub32 {
                dest: ident!("var8"),
                val1: Val::Ident(ident!("var9")),
                val2: Val::Ident(ident!("var10")),
            }, // var8 = var9 - var10
            IRIns::Add32 {
                dest: ident!("var11"),
                val1: Val::Ident(ident!("var12")),
                val2: Val::Ident(ident!("var13")),
            }, // var11 = var12 + var13
            IRIns::Sub32 {
                dest: ident!("var14"),
                val1: Val::Ident(ident!("var15")),
                val2: Val::Ident(ident!("var16")),
            }, // var14 = var15 - var16
            IRIns::Add32 {
                dest: ident!("var17"),
                val1: Val::Ident(ident!("var18")),
                val2: Val::Ident(ident!("var19")),
            }, // var17 = var18 + var19
            IRIns::Sub32 {
                dest: ident!("var20"),
                val1: Val::Ident(ident!("var1")),
                val2: Val::Ident(ident!("var2")),
            }, // var20 = var1 - var2
            // Final print to check final values
            IRIns::Print32 {
                val: Val::Ident(ident!("var1")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var2")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var3")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var4")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var5")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var6")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var7")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var8")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var9")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var10")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var11")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var12")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var13")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var14")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var15")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var16")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var17")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var18")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var19")),
            },
            IRIns::Print32 {
                val: Val::Ident(ident!("var20")),
            },
        ]);

        let f = encode!(no_disasm, inss => extern "C" fn(extern "C" fn (i32) -> ()) -> i32);
        let result = f(fc_ptr);

        drop(fc_);

        // Expected output
        let expected_output = vec![
            5, 4, 3, -1, 23, 6, 17, 10, 9, -1, 41, 12, 29, 16, 15, -1, 43, 18, 25, 1,
        ];

        // Validate the output
        assert_eq!(output, expected_output);

        println!("outputs = {output:?}");
    }
}
