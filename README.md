# x86-codegen
##### *Work in progress, nowhere close to completion*
An experimental x86 code generator written in Rust. Consists of a dynamic assembler and an IR stage.

### Progress
- [ ] Dynamic assembler with encoder generator
  - [ ] Instruction field support
    - [x] REX
    - [x] ModRM
    - [ ] SIB
    - [x] VEX2
    - [x] VEX3
    - [x] immediate bytes
  - [ ] Compile time code generation for constant bytes
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
