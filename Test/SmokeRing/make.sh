#!/bin/sh

if [ ! -e "Makefile.kvs" ]; then
    kvsmake -G
fi

kvsmake
