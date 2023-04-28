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
