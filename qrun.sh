#!/bin/sh
rasm/rasm $1 rasm/prog.rexe && rsim/rsim -q rasm/prog.rexe

