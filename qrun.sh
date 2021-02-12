#!/bin/bash
rasm/rasm $1 rasm/prog.rexe && rsim/rsim 4 rasm/prog.rexe

