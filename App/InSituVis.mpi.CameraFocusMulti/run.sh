#!/bin/sh
PROGRAM=${PWD##*/}
PARAMS_FILE="params.namelist"

# macOS: raise stack size to avoid segmentation faults from large local arrays.
if [ "$(uname -s)" = "Darwin" ]; then
    HARD_STACK=`ulimit -H -s 2>/dev/null`
    if [ -n "$HARD_STACK" ]; then
        ulimit -S -s "$HARD_STACK" 2>/dev/null || true
    fi
fi

DATA_DIR=`cat $PARAMS_FILE | grep 'Data_dir_name' | awk '{print substr($4, 2, length($4)-2)}'`
RESTART_DIR="restart"
VIS2D_DIR="vis2d"

mkdir -p "$DATA_DIR/$RESTART_DIR" "$DATA_DIR/$VIS2D_DIR"

NPROC_X=`cat ./make.sh | grep '^NPROC_X=' | awk -F "=" '{print $2}' | awk '{print $1}'`
NPROC_Y=`cat ./make.sh | grep '^NPROC_Y=' | awk -F "=" '{print $2}' | awk '{print $1}'`
NPROC_Z=`cat ./make.sh | grep '^NPROC_Z=' | awk -F "=" '{print $2}' | awk '{print $1}'`
NPROCS=`echo "$NPROC_X * $NPROC_Y * $NPROC_Z" | bc`

MPIEXEC=${MPIEXEC:-mpiexec}
MPI_OPTIONS=${MPI_OPTIONS:-}

# Open MPI: allow running more MPI ranks than detected slots.
if [ -z "$MPI_OPTIONS" ] && $MPIEXEC --version 2>/dev/null | grep -qi "Open MPI"; then
    MPI_OPTIONS="--oversubscribe"
fi

$MPIEXEC $MPI_OPTIONS -np "$NPROCS" ./"$PROGRAM" "$PARAMS_FILE"
