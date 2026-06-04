#!/bin/sh
set -eu

if [ -n "${KVS_DIR:-}" ]; then
    PATH="${KVS_DIR}/bin:${PATH}"
    export PATH
fi

if ! command -v kvsmake >/dev/null 2>&1; then
    echo "kvsmake was not found. Set KVS_DIR and add KVS_DIR/bin to PATH." >&2
    exit 1
fi

if [ ! -e Makefile.kvs ]; then
    kvsmake -G
fi

kvsmake "$@"
