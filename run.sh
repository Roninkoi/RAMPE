#!/bin/sh
# Assemble .rasm file and run fast
# Usage: ./run.sh <prog.rasm>
./rasm/rasm $1 ./rasm/prog.rexe && ./rsim/rsim ./rasm/prog.rexe

