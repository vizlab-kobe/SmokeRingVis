#!/bin/sh

if [ ! -e "Makefile.kvs" ]; then
    kvsmake -G -use_mpi
fi

kvsmake
