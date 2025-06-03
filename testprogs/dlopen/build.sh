#!/bin/sh

BASEDIR=$(dirname "$0")
gcc -g -O2 -shared -fPIC -o "$BASEDIR/dl.so" "$BASEDIR/dl.c"
gcc -g -O2 -o "$BASEDIR/main" "$BASEDIR/main.c"
