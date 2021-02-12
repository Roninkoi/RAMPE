#!/bin/bash
rasm/rasm $1 rasm/prog.rexe && rsim/rsim 2 rasm/prog.rexe

