#!/bin/sh
# Assemble .rasm file and single-step run
# Usage: ./srun.sh <prog.rasm>
./rasm/rasm $1 ./rasm/prog.rexe && ./rsim/rsim -s ./rasm/prog.rexe

