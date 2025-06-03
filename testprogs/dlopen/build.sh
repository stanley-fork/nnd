#!/bin/sh

BASEDIR=$(dirname "$0")
gcc -g -O2 -shared -fPIC -o "$BASEDIR/build/dl.so" "$BASEDIR/dl.c"
gcc -g -O2 -o "$BASEDIR/build/main" "$BASEDIR/main.c"
