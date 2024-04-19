#!/bin/sh
set -e

BASEDIR=$(dirname "$0")
gcc -g -O2 -c -o "$BASEDIR/a.o" "$BASEDIR/a.c"
gcc -g -O2 -c -o "$BASEDIR/b.o" "$BASEDIR/b.c"
gcc -g -O2 -c -o "$BASEDIR/main.o" "$BASEDIR/main.c"
gcc -g -O2 -o "$BASEDIR/main" "$BASEDIR/main.o" "$BASEDIR/a.o" "$BASEDIR/b.o"
