#!/bin/sh
PS_COUNT=`ps aux | grep InSituVis.mpi | grep -v grep | wc -l`
if [ $PS_COUNT -gt 0 ]; then
    killall InSituVis.mpi
fi

if [ -e "Makefile.kvs" ]; then
    kvsmake distclean
fi

OUTPUT_DIR=Output
if [ -e $OUTPUT_DIR ]; then
    rm -rf $OUTPUT_DIR
fi

PARAMS_FILE="params.namelist"
DATA_DIR=`cat $PARAMS_FILE | grep 'Data_dir_name' | awk '{print substr($4, 2, length($4)-2)}'`
if [ -e $DATA_DIR ]; then
    rm -rf $DATA_DIR
fi
