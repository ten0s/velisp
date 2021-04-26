#!/bin/bash -e

dir=$(dirname $0)

for test in `ls ${dir}/*.exp`; do
	if [[ -x $test ]]; then
        echo
        echo "----------------------"
        echo "Running ${test}"
        echo "----------------------"
        $test
        echo
        echo "----------------------"
        echo "Success"
        echo "----------------------"
    fi
done
