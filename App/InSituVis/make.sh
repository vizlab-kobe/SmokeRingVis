#!/bin/bash

NX=92
NY=32
NZ=32

# ==============================================================================
# 'sed -i' command
# ==============================================================================
shopt -s expand_aliases
if sed --version 2>/dev/null | grep -q GNU; then
    alias sedi='sed -i '
else
    alias sedi='sed -i "" '
fi

# ==============================================================================
#  Update 'constants.F90'
# ==============================================================================
SRLIB_DIR=../../Lib.smoke_ring_m
CONSTANTS_FILE=${SRLIB_DIR}/constants.f90

RECOMPILE=0

SRC_NX=`cat ${CONSTANTS_FILE} | grep ":: NX =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NX} -ne ${SRC_NX} ]; then
    sedi -e "s/:: NX =  *${SRC_NX}/:: NX = ${NX}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

SRC_NY=`cat ${CONSTANTS_FILE} | grep ":: NY =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NY} -ne ${SRC_NY} ]; then
    sedi -e "s/:: NY =  *${SRC_NY}/:: NY = ${NY}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

SRC_NZ=`cat ${CONSTANTS_FILE} | grep ":: NZ =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NZ} -ne ${SRC_NZ} ]; then
    sedi -e "s/:: NZ =  *${SRC_NZ}/:: NZ = ${NZ}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

if [ ${RECOMPILE} -eq 1 ]; then
    cd ${SRLIB_DIR}
    python3 ./kvsmake.py rebuild
    cd -
fi

# ==============================================================================
#  KVS make
# ==============================================================================
if [ ! -e "Makefile.kvs" ]; then
    kvsmake -G
fi

if [ ${RECOMPILE} -eq 1 ]; then
    kvsmake rebuild
else
    kvsmake
fi
