#!/usr/bin/env bash

export VAGRANT_VAGRANTFILE=Vagrantfile.macos-catalina.build

vagrant up --provision
if [[ $? -ne 0 ]]; then
    echo "Build Failed"
    exit 1
fi

vagrant ssh -c 'source ~/.zshrc; cd velisp; macos/build.sh'
if [[ $? -eq 0 ]]; then
    echo "Build Done"
else
    echo "Build Failed"
    exit 2
fi
