#!/usr/bin/env bash

export VAGRANT_VAGRANTFILE=Vagrantfile.macos.deps
export MACOS_NAME=big_sur

vagrant up --provision
if [[ $? -ne 0 ]]; then
    echo "Bootstrap Failed"
    exit 1
fi

echo "Bootstrap Done"
