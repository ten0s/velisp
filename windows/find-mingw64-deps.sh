#!/bin/bash

#
# See how to determine what DLLs and Typelibs are needed
# https://ten0s.github.io/blog/2022/07/01/debugging-dll-loading-errors
# https://ten0s.github.io/blog/2022/07/25/find-dlls-and-typelibs-dependencies-for-nodejs-gtk-application-on-windows
#

function is-sls-enabled() {
    IMAGE=$1
    reg query "HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Image File Execution Options\\$IMAGE" | grep -Eq "GlobalFlag\s+REG_SZ\s+0x00000002"
}

function is-cdb-available() {
    cdb &>/dev/null
    if [[ $? -eq 2 ]]; then
        return 0
    else
        return 1
    fi
}

function copy-dlls() {
    LOG=$1
    SRC=$2
    DST=$3
    cat $LOG | sed -En 's/.*Unable to load DLL: "([^"]*\.dll)".*/\1/p' | while read dll; do
        if [[ -f $SRC/$dll ]]; then
            cp -v $SRC/$dll $DST/
        fi
    done
}

function copy-typelibs() {
    LOG=$1
    SRC=$2
    DST=$3
    cat $LOG | sed -En "s/.*Typelib file for namespace '([^']+)'.*/\1/p" | while read typelib; do
        cp -v $SRC/$typelib* $DST/
    done
}

if [[ $# -lt 1 ]]; then
    echo "Usage: $(basename $0) PROG [ARG...]"
    exit 1
fi

PROG=$1
shift
ARGS="$@"

if ! is-sls-enabled $PROG; then
    echo "Enable Show Loader Snaps (sls) for $PROG and try again"
    exit 1
fi

if ! is-cdb-available; then
    echo "CDB is not found"
    echo "Run the command below and try again"
    echo 'export PATH="/c/Program Files (x86)/Windows Kits/10/Debuggers/x64/":$PATH'
    exit 1
fi

mkdir -p mingw64/{bin,lib/girepository-1.0}

TEMP=$(mktemp)
while true; do
    cdb -c "g;q" $PROG $ARGS &>$TEMP
    if [[ $? -ne 0 ]]; then
        copy-dlls $TEMP /mingw64/bin/ mingw64/bin/
        copy-typelibs $TEMP /mingw64/lib/girepository-1.0/ mingw64/lib/girepository-1.0/
    else
        break
    fi
done
rm $TEMP

ls mingw64/bin/                  | xargs -0 > windows/mingw64-dlls.txt
ls mingw64/lib/girepository-1.0/ | xargs -0 > windows/mingw64-typelibs.txt

exit 0
