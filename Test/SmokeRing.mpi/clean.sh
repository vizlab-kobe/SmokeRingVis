#!/bin/sh
PS_COUNT=`ps aux | grep SmokeRing.mpi | grep -v grep | wc -l`
if [ $PS_COUNT -gt 0 ]; then
    killall SmokeRing.mpi
fi

if [ -e "Makefile.kvs" ]; then
    kvsmake distclean
fi
