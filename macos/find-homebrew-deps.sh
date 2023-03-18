#!/usr/bin/env bash -e

if [[ $# -eq 0 ]]; then
    echo "usage: $(basename $0) PROG [ARGS...]"
    exit 1
fi

BASE_DIR=$(dirname $0)

PROG="$1"
shift
ARGS="$@"

unset DYLD_LIBRARY_PATH
unset GI_TYPELIB_PATH

${PROG} ${ARGS} &
PROG_PID=$!

# TODO: add -t | --timeout option
sleep 10

TMP=$(mktemp)
lsof -p $PROG_PID > $TMP
cat $TMP | egrep '/usr/local/Cellar/.*\.dylib'   | awk '{ print $9 }' | sort -u > dylibs.txt
cat $TMP | egrep '/usr/local/Cellar/.*\.typelib' | awk '{ print $9 }' | sort -u > typelibs.txt
rm $TMP

kill $PROG_PID >& /dev/null

DYLIB_PATH=$PWD/homebrew/lib
TYPELIB_PATH=$PWD/homebrew/lib/girepository-1.0

mkdir -p $DYLIB_PATH $TYPELIB_PATH

# Copy and store dylibs and their reverse links
for dylib in $(cat dylibs.txt); do
    #echo $dylib
    cp -f -R $dylib $DYLIB_PATH/

    # Find reverse links pointing to dylib
    dylib_dir=$(dirname $dylib)
    dylib_name=$(basename $dylib)
    for link in $(find $dylib_dir -type link); do
        #echo $link
        target_name=$(basename $(realpath $link))
        if [[ $target_name == $dylib_name ]]; then
            cp -f -R $link $DYLIB_PATH/
            echo $link >> dylibs.txt
        fi
    done
done
# Sort dylibs and their reverse links
TEMP=$(mktemp); sort -u dylibs.txt > $TEMP; mv $TEMP dylibs.txt

# Copy typelibs
for typelib in $(cat typelibs.txt); do
    cp -f -R $typelib $TYPELIB_PATH/
done

# See also copy-homebrew-deps.sh

touch $BASE_DIR/homebrew-dylibs.txt
touch $BASE_DIR/homebrew-typelibs.txt

if ! diff $BASE_DIR/homebrew-dylibs.txt dylibs.txt; then
    mv -f dylibs.txt $BASE_DIR/homebrew-dylibs.txt
    echo "$BASE_DIR/homebrew-dylibs.txt updated"
else
    rm dylibs.txt
fi

if ! diff $BASE_DIR/homebrew-typelibs.txt typelibs.txt; then
    mv -f typelibs.txt $BASE_DIR/homebrew-typelibs.txt
    echo "$BASE_DIR/homebrew-typelibs.txt updated"
else
    rm typelibs.txt
fi
