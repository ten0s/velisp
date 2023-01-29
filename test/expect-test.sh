#!/bin/bash -e

dir=$(dirname $0)

for test in ${dir}/*.exp ${dir}/stacktrace/*/*.exp; do
    echo
    echo "----------------------"
    echo "Running ${test}"
    echo "----------------------"
    expect $test
    echo
    echo "----------------------"
    echo "Success"
    echo "----------------------"
done
