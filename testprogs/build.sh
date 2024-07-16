#!/bin/sh
set -e

BASEDIR=$(dirname "$0")

for name in `ls "$BASEDIR" | grep -P '\.c$'`
do
    name_without_extension="${name%.*}"
    echo $name_without_extension
    clang -g -O1 -std=c23 -pthread -o "$BASEDIR/$name_without_extension" "$BASEDIR/$name"
done

FLAGS="-g -O0 -std=c++20 -Wno-vla-cxx-extension"
for name in `ls "$BASEDIR" | grep -P '\.cpp$'`
do
    name_without_extension="${name%.*}"
    echo $name_without_extension
    clang++ -stdlib=libc++ $FLAGS -o "$BASEDIR/$name_without_extension-libc++" "$BASEDIR/$name"
    clang++ -stdlib=libstdc++ $FLAGS -o "$BASEDIR/$name_without_extension" "$BASEDIR/$name"
done

for name in `ls "$BASEDIR" | grep -P '\.rs$'`
do
    name_without_extension="${name%.*}"
    echo $name_without_extension
    rustc --edition=2021 -g -o "$BASEDIR/$name_without_extension" "$BASEDIR/$name"
done
