#!/bin/bash

mkdir -p install

docker build --build-arg GID=$(id -g) --build-arg UID=$(id -u) --tag velisp .

docker run -ti --rm -v$PWD:/home/user/velisp/install velisp bash -c 'cp velisp-*-*-* install/'

echo "Build Done"
