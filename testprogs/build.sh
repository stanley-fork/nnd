#!/bin/sh
set -e
#set -x

BASEDIR=$(dirname "$0")

if [ ! -d "$BASEDIR/build" ]
then
    mkdir "$BASEDIR/build"
fi

build_file() {
    local file="$1"
    local name_without_extension="${file%.*}"
    local extension="${file##*.}"

    case "$extension" in
        c)
            echo "$file"
            clang -g -O1 -std=c23 -pthread -mavx -mavx2 -o "$BASEDIR/build/$name_without_extension" "$BASEDIR/$file"
            ;;
        cpp)
            echo "$file"
            FLAGS="-g -O1 -std=c++20 -Wno-vla-cxx-extension -mavx -mavx2"
            clang++ -stdlib=libc++ $FLAGS -o "$BASEDIR/build/$name_without_extension-libc++" "$BASEDIR/$file"
            clang++ -stdlib=libstdc++ $FLAGS -o "$BASEDIR/build/$name_without_extension" "$BASEDIR/$file"
            ;;
        rs)
            echo "$file"
            rustc --edition=2021 -g -o "$BASEDIR/build/$name_without_extension" "$BASEDIR/$file"
            ;;
        zig)
            echo "$file"
            zig build-exe -femit-bin="$BASEDIR/build/$name_without_extension" "$BASEDIR/$file"
            zig build-exe -fno-llvm -fno-lld -femit-bin="$BASEDIR/build/${name_without_extension}-nollvm" "$BASEDIR/$file"
            rm -f "$BASEDIR/build/${name_without_extension}.o" "$BASEDIR/build/${name_without_extension}-nollvm.o"
            ;;
    esac
}

if [ $# -gt 0 ]; then
    for filename in "$@"; do
        p="$BASEDIR/$filename"
        if ! [ -f "$p" ]
        then
            echo "file $p not found"
            exit 1
        fi
        build_file "$(basename "$filename")"
    done
else
    for file in "$BASEDIR"/*; do
        filename=$(basename "$file")
        build_file "$filename"
    done
fi
