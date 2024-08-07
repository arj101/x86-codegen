# x86-codegen
##### *Work in progress, nowhere close to completion*
An experimental x86 code generator written in Rust. Consists of a dynamic assembler and an IR stage.

### Progress
- [ ] Dynamic assembler with encoder generator
  - [ ] Encoder generator
    - [x] Instruction field support
      - [x] REX
      - [x] ModRM
      - [x] SIB
      - [x] VEX2
      - [x] VEX3
      - [x] immediate bytes
      - [ ] Legacy prefixes?
    - [x] Labels
      - [x] rel32 jumps 
      - [ ] Other types of jumps?
    - [x] Memory alignment padding pseudo instruction
    - [ ] Compile time code generation for constant bytes
  - [ ] Dynamic Assembler
    - [x] Basic instruction encoders
    - [ ] Support for more 64bit instructions
    - [ ] SETcc instructions for logical expression evaluation
    - [ ] SSE instructions
- [ ] IR
  - [ ] Expression evaluater
    - [x] Arithmetic expressions
    - [ ] Logical and relational expressions
      - [ ] AND, OR
      - [ ] <=, <, ==, >, >=, !=
    - [ ] Expression simplification
  - [ ] Statement builder
    - [ ] Branching constructs
    - [ ] Looping constructs
  - [ ] Optimizers
