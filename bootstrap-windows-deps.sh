#!/usr/bin/env bash

export VAGRANT_VAGRANTFILE=Vagrantfile.win10.deps

vagrant up --provision
if [[ $? -ne 0 ]]; then
    echo "Bootstrap Failed"
    exit 1
fi

echo "Bootstrap Done"
