#!/bin/bash
rasm/rasm $1 rasm/prog.rexe && rsim/rsim 3 rasm/prog.rexe

