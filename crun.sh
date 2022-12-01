#!/bin/sh
# Assemble .rasm file and run using 10 Hz clock
# Usage: ./crun.sh <prog.rasm>
./rasm/rasm $1 ./rasm/prog.rexe && ./rsim/rsim -c ./rasm/prog.rexe

