#!/bin/sh
PROGRAM=${PWD##*/}
PARAMS_FILE="params.namelist"

./$PROGRAM ${PARAMS_FILE} 1
