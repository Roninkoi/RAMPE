# RAKIAC - Relay-Architectured Keeper of Information And Calculator

The RAKIAC is a relay computer with a simple ISA that I designed.
The purpose of this project is to explore early computing and CPU design.

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

mar = memory address register

## List of instructions (see documentation)

v = value, a = address, r = register

```
nop
hlt
end
out

atm
mta
atp
pta
lda
sta
inc
dec

sw v
jmp a
jez a
jlz a

mov r, r
sto a
ld a
ldi v

not r
and r, r
or r, r
xor r, r

add r, r
sub r, r
sh v, v
shr v
shl v
```
