#!/bin/sh
# Assemble .rasm file and run quietly
# Usage: ./qrun.sh <prog.rasm>
rasm/rasm $1 rasm/prog.rexe && rsim/rsim -q rasm/prog.rexe

