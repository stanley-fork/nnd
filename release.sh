#!/bin/bash
set -e

if [ "$1" != "allow-uncommitted" ]
then
    status="$(git status)"
    if [ "$status" != "" ]
    then
        echo "uncommitted changes"
        exit 1
    fi
fi

RUST_BACKTRACE=1 cargo test && cargo build --profile dbgo && cargo build -r

scp target/x86_64-unknown-linux-musl/dbgo/nnd dev:bin/new-nnd && scp target/x86_64-unknown-linux-musl/release/nnd dev:bin/new-nnd-release && ssh dev 'mv bin/new-nnd bin/nnd; mv bin/new-nnd-release bin/nnd-release;'

cp target/x86_64-unknown-linux-musl/dbgo/nnd ../../nnd-release/nnd-new && mv ../../nnd-release/nnd-new ../../nnd-release/nnd && cp target/x86_64-unknown-linux-musl/release/nnd ../../nnd-release/nnd-release-new && mv ../../nnd-release/nnd-release-new ../../nnd-release/nnd-release

butler push target/x86_64-unknown-linux-musl/release/nnd al13n/nnd:linux

TAG_NUM=`git describe --tags --abbrev=0 | grep -Po '(?<=^v0\.)\d+$' | sort -n | tail -n1`
if [ "$TAG_NUM" == "" ]
then
    echo "couldn't find latest tag number"
fi
TAG="v0.$TAG_NUM"

if gh release view $TAG >/dev/null
then
    TAG_NUM=$(($TAG_NUM + 1))
    TAG="v0.$TAG_NUM"
    git tag "$TAG"
    git push origin "$TAG"
fi

cp target/x86_64-unknown-linux-musl/dbgo/nnd nnd-dbgo
trap 'rm nnd-dbgo' EXIT

gh release create "$TAG" --notes "" target/x86_64-unknown-linux-musl/release/nnd nnd-dbgo

echo "all done!"
