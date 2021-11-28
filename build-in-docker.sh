#!/bin/bash

mkdir -p install

docker build -t velisp .

docker run -ti --rm -v$PWD:/home/user/velisp/install velisp bash -c 'cp velisp-*-*-* install/'

echo "Build Done"
