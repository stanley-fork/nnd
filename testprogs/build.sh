#!/bin/sh
set -e

BASEDIR=$(dirname "$0")
for name in `ls "$BASEDIR" | grep -P '\.(c|cpp)$'`
do
    name_without_extension="${name%.*}"
    echo $name_without_extension
    g++ -g -O1 -std=c++20 -fcoroutines -o "$BASEDIR/$name_without_extension" "$BASEDIR/$name"
done

for name in `ls "$BASEDIR" | grep -P '\.rs$'`
do
    name_without_extension="${name%.*}"
    echo $name_without_extension
    rustc --edition=2021 -g -o "$BASEDIR/$name_without_extension" "$BASEDIR/$name"
done
