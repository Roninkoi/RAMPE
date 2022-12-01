#!/bin/sh
# Assemble .rasm file into executable .rexe
# Usage: ./assemble.sh <prog.rasm>
./rasm/rasm $1 ./rasm/prog.rexe
