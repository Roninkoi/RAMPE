#!/bin/sh
rasm/rasm $1 rasm/prog.rexe && rsim/rsim -c rasm/prog.rexe

