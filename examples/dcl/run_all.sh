#!/bin/bash

for file in `ls ${0%/*}/*.dcl`; do
    dir=$(dirname $file)
    base=$(basename $file)
    name=${base%%.dcl}
    ${dir}/run_dlg.sh ${name}
done
