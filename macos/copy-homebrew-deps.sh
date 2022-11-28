#!/usr/bin/env bash

if [[ $# -ne 1 ]]; then
    echo "Usage: $(basename $0) <DEST_DIR>"
    exit 1
fi

BASE_DIR=$(dirname $0)
DEST_DIR=$1

DYLIB_PATH=homebrew/lib
TYPELIB_PATH=homebrew/lib/girepository-1.0

# See also find-homebrew-deps.sh

mkdir -p $DEST_DIR/$DYLIB_PATH
cat $BASE_DIR/homebrew-dylibs.txt | \
    xargs -I'{}' cp -R {} $DEST_DIR/$DYLIB_PATH

mkdir -p $DEST_DIR/TYPELIB_PATH
cat $BASE_DIR/homebrew-typelibs.txt | \
    xargs -I'{}' cp -R {} $DEST_DIR/$TYPELIB_PATH

exit 0
