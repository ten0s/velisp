#!/bin/bash

for file in `ls ${0%/*}/*.dcl`; do
    dir=$(dirname $file)
    base=$(basename $file)
    name=${base%%.dcl}
    echo Running ${name}...
    ${dir}/run_dlg.sh ${name}
    echo
done
