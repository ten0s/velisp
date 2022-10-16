#!/bin/bash -e

#
# See how to determine what DLLs and Typelibs are needed
# https://ten0s.github.io/blog/2022/07/01/debugging-dll-loading-errors
# https://ten0s.github.io/blog/2022/07/25/find-dlls-and-typelibs-dependencies-for-nodejs-gtk-application-on-windows
#

if [[ $# -ne 1 ]]; then
    echo "Usage: $(basename $0) <DEST_DIR>"
    exit 1
fi

BASE_DIR=$(dirname $0)
DEST_DIR=$1

mkdir -p $DEST_DIR/mingw64/bin
cat $BASE_DIR/mingw64-dlls.txt | \
    xargs -I'{}' cp /mingw64/bin/{} $DEST_DIR/mingw64/bin/

mkdir -p $DEST_DIR/mingw64/lib/girepository-1.0
cat $BASE_DIR/mingw64-typelibs.txt | \
    xargs -I'{}' cp /mingw64/lib/girepository-1.0/{} $DEST_DIR/mingw64/lib/girepository-1.0/

exit 0
