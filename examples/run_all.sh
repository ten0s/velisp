#!/bin/bash

for file in `ls ${0%/*}/*.dcl`; do
    echo ${file%%.dcl}.sh
    ${file%%.dcl}.sh
done
