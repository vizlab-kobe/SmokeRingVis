#!/bin/sh
PROGRAM=${PWD##*/}
PARAMS_FILE="params.namelist"

DATA_DIR=`cat $PARAMS_FILE | grep 'Data_dir_name' | awk '{print substr($4, 2, length($4)-2)}'`
RESTART_DIR="restart"
VIS2D_DIR="vis2d"

if [ ! -d $DATA_DIR ]; then
    mkdir $DATA_DIR
    mkdir $DATA_DIR"/"$RESTART_DIR
    mkdir $DATA_DIR"/"$VIS2D_DIR
fi

NPROC_X=`cat ./make.sh | grep '^NPROC_X=' | awk -F "=" '{print $2}' | awk '{print $1}'`
NPROC_Y=`cat ./make.sh | grep '^NPROC_Y=' | awk -F "=" '{print $2}' | awk '{print $1}'`
NPROC_Z=`cat ./make.sh | grep '^NPROC_Z=' | awk -F "=" '{print $2}' | awk '{print $1}'`
NPROCS=`echo "$NPROC_X * $NPROC_Y * $NPROC_Z" | bc`

mpiexec -np $NPROCS ./$PROGRAM $PARAMS_FILE
