#!/bin/sh

NPROC_X=2
NPROC_Y=1
NPROC_Z=1

NXPP=40
NYPP=40
NZPP=40


# ==============================================================================
#  Update 'constants.F90'
# ==============================================================================
SRLIB_DIR=../../Lib.smoke_ring_m.mpi
CONSTANTS_FILE=${SRLIB_DIR}/constants.F90

RECOMPILE=0

SRC_NPROC_X=`cat ${CONSTANTS_FILE} | grep ":: NPROC_X =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NPROC_X} -ne ${SRC_NPROC_X} ]; then
    sed -i "" -e "s/:: NPROC_X =  *${SRC_NPROC_X}/:: NPROC_X = ${NPROC_X}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

SRC_NPROC_Y=`cat ${CONSTANTS_FILE} | grep ":: NPROC_Y =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NPROC_Y} -ne ${SRC_NPROC_Y} ]; then
    sed -i "" -e "s/:: NPROC_Y =  *${SRC_NPROC_Y}/:: NPROC_Y = ${NPROC_Y}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

SRC_NPROC_Z=`cat ${CONSTANTS_FILE} | grep ":: NPROC_Z =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NPROC_Y} -ne ${SRC_NPROC_Y} ]; then
    sed -i "" -e "s/:: NPROC_Z =  *${SRC_NPROC_Z}/:: NPROC_Z = ${NPROC_Z}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

SRC_NXPP=`cat ${CONSTANTS_FILE} | grep ":: NXPP =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NXPP} -ne ${SRC_NXPP} ]; then
    sed -i "" -e "s/:: NXPP =  *${SRC_NXPP}/:: NXPP = ${NXPP}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

SRC_NYPP=`cat ${CONSTANTS_FILE} | grep ":: NYPP =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NYPP} -ne ${SRC_NYPP} ]; then
    sed -i "" -e "s/:: NYPP =  *${SRC_NYPP}/:: NYPP = ${NYPP}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

SRC_NZPP=`cat ${CONSTANTS_FILE} | grep ":: NZPP =" | awk -F "=" '{print $2}' | awk '{print $1}'`
if [ ${NZPP} -ne ${SRC_NZPP} ]; then
    sed -i "" -e "s/:: NZPP =  *${SRC_NZPP}/:: NZPP = ${NZPP}/g" ${CONSTANTS_FILE}
    RECOMPILE=1
fi

if [ ${RECOMPILE} -eq 1 ]; then
    cd ${SRLIB_DIR}
    rm *.o *.mod
    kvsmake lib
    cd -
fi

# ==============================================================================
#  KVS make
# ==============================================================================
if [ ! -e "Makefile.kvs" ]; then
    kvsmake -G -use_mpi
fi

if [ ${RECOMPILE} -eq 1 ]; then
    kvsmake rebuild
else
    kvsmake
fi
