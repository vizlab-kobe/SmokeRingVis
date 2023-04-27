#!/bin/sh

kvsmake distclean

OUTPUT_DIR=Output
if [ -e $OUTPUT_DIR ]; then
    rm -rf $OUTPUT_DIR
fi
