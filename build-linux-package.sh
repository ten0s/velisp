#!/bin/bash

mkdir -p install

docker build --build-arg GID=$(id -g) --build-arg UID=$(id -u) --tag velisp .
if [[ $? -ne 0 ]]; then
    echo "Build Failed"
    exit 1
fi

docker run -ti --rm -v$PWD:/home/user/velisp/install velisp bash -c 'cp velisp-*-*-*.tar.gz install/'
if [[ $? -eq 0 ]]; then
    echo "Build Done"
else
    echo "Build Failed"
    exit 2
fi
