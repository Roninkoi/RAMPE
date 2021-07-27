# RAKIAC - Relay-Architectured Keeper of Information And Calculator

The RAKIAC is a relay computer with a simple custom ISA.
The purpose of this project is to explore early computing and CPU design.

The architecture is going to change substatially as the number of components is optimized.

Included are the RASM assembler and RSIM simulator. Both are written in Fortran.

## Assembler

Usage: rasm input.rasm output.rexe

![asmscreen](https://user-images.githubusercontent.com/12766039/68814179-d063bf80-0680-11ea-8f71-9be1fd8b7e75.png)

## Simulator

Usage: rsim mode program.rexe

![simscreen](https://user-images.githubusercontent.com/12766039/68814178-ce99fc00-0680-11ea-9394-60347a9ef967.png)

#### Modes:

0 = run from stdin

1 = run from file

2 = single step from file (press enter)

3 = slow run from file (10 Hz clock)

4 = quietly run from file (only out instructions)

## Registers

a = accumulator (00 in binary)

b = general purpose register (01)

c = gpr (10)

d = gpr (11)

pc = program counter

ir = instruction register

## Basic instructions (see documentation)

v = value, a = address, r = register

| Instruction | Purpose |
| ----------- | ------- |
| nop | no operation |
| hlt | halt |
| in | read |
| out | print |
| inc | increment acc |
| dec | decrement acc |
| jmp r, r | jump |
| jez r, r | jump if register equals zero |
| jlz r, r | jump if register less than zero |
| mov r, r | move register to register |
| sto r, r | store register to address |
| ld r, r | load address to register |
| ll v | load immediate to al |
| lh v | load immediate to ah |
| not r | logical not |
| and r, r | logical and |
| or r, r | logical or |
| xor r, r | logical xor |
| add r, r | addition |
| sub r, r | subtraction |
| sh v, v | logical shift |
