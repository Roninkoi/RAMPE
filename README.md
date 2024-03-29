# RAMPE - Relay-Architectured Memorizer, Processor and Enumerator

The RAMPE is a relay computer with a simple custom ISA.
The purpose of this project is to explore early computing and CPU design.

Included are the RASM assembler and RSIM simulator. Both are written in Fortran.

## Assembler

Usage: `rasm <input.rasm> <output.rexe>`

```
   1 |  0,  0 |  11000000  xor a, a                        
   2 |  0,  1 |  01110110  ll loop                         
   3 |  0,  2 |  01000100  mov b, a ; loop address in b    
   4 |  0,  3 |  01111001  ll exit                         
   5 |  0,  4 |  01001000  mov c, a ; exit address in c    
   6 |  0,  5 |  11000000  xor a, a                        
   7 |  0,  6 |  00001000  loop: inc                       
   8 |  0,  7 |  00110010  jlz a, c                        
   9 |  0,  8 |  00010100  jmp b                           
  10 |  0,  9 |  00000001  exit: hlt                       
```

## Simulator

Usage: `rsim [mode] [program.rexe]`

`rsim` = run instructions from stdin

`rsim <file>` = run from file

Modes:

-s = single step from file (press enter)

-c = slow run from file (10 Hz clock)

-q = quietly run from file (only out instructions)

```
cycle: 0
a:   00000000 (0)
b:   00000000 (0)
c:   00000000 (0)
d:   00000000 (0)
ir:  11000000 (192)
pc:  00000001 (1)
cycle: 1
a:   00000110 (6)
b:   00000000 (0)
c:   00000000 (0)
d:   00000000 (0)
ir:  01110110 (118)
pc:  00000010 (2)
```

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
| get | pc address to acc |
| set | acc to pc address |
| sw | switch memory bank and jump |
| in | read |
| out | print |
| inc | increment acc |
| dec | decrement acc |
| jmp r | jump |
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

## Assembler pseudoinstructions

l = label

| Instruction | Purpose |
| ----------- | ------- |
| la l | load label address into a |
| lb l | load label bank into a |
| lba l | load label bank and address into b and a |
| ja l | jump to label address |
| jea l | jump to label if a equals zero |
| jla l | jump to label if a less than zero |
| lda l | load from label address into a |
| sta l | store from a into label address |
| shr r | shift right |
| shrr r | shift right twice |
| shl r | shift left |
| shll r | shift left twice |

Other registers have corresponding instructions such as jeb, jlb, ldb, stb for b register.

