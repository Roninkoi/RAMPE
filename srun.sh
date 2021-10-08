#!/bin/sh
rasm/rasm $1 rasm/prog.rexe && rsim/rsim -s rasm/prog.rexe

