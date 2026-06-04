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

./$PROGRAM ${PARAMS_FILE} 1
