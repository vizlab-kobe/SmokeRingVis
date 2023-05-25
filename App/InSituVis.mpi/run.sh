#!/bin/sh
PROGRAM=${PWD##*/}
PARAMS_FILE="params.namelist"

mpiexec -np 8 ./${PROGRAM} ${PARAMS_FILE}
