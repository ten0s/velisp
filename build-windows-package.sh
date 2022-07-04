#!/bin/bash

export VAGRANT_VAGRANTFILE=Vagrantfile.win10.build

vagrant up --provision
if [[ $? -ne 0 ]]; then
    echo "Build Failed"
    exit 1
fi

vagrant winrm -s cmd -c 'C:\\msys64\\msys2_shell.cmd -defterm -full-path -no-start -mingw64 -c "cd velisp; windows/build.sh"'
if [[ $? -eq 0 ]]; then
    echo "Build Done"
else
    echo "Build Failed"
    exit 2
fi
