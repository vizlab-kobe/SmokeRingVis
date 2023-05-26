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

mpiexec -n 2 ./$PROGRAM $PARAMS_FILE
