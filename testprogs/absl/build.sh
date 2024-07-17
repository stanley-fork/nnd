#!/bin/sh
set -e

BASEDIR=$(dirname "$0")

mkdir -p "$BASEDIR/lib"
if [ -z "$(ls -A "$BASEDIR/lib")" ]
then
    git clone git@github.com:abseil/abseil-cpp.git "$BASEDIR/lib"
fi
mkdir -p "$BASEDIR/build"
(
    cd "$BASEDIR/build"
    cmake ..
    make -j8 containers
)
