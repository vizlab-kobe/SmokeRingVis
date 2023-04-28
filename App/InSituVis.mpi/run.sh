#!/bin/sh
PROGRAM=${PWD##*/}
PARAMS_FILE="params.namelist"

mpiexec -np 12 ./$PROGRAM ${PARAMS_FILE}
