#!/bin/bash

for file in `ls ${0%/*}/*.dcl`; do
    ${file%%.dcl}.sh
done
