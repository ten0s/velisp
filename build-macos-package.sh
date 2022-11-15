#!/usr/bin/env bash

export VAGRANT_VAGRANTFILE=Vagrantfile.macos-catalina.build

vagrant up --provision
if [[ $? -ne 0 ]]; then
    echo "Build Failed"
    exit 1
fi

vagrant ssh -c 'source ~/.zshrc; cd velisp; macos/build.sh'
if [[ $? -eq 0 ]]; then
    vagrant ssh-config | awk '
        /HostName /     { host = $2 }
        /Port /         { port = $2 }
        /User /         { user = $2 }
        /IdentityFile / { id   = $2 }
        END             { printf("rsync -vv -e \"ssh -p%s -i%s\" %s@%s:velisp/velisp-*-*-*.tar.gz .\n", port, id, user, host) }
    ' | bash
    if [[ $? -eq 0 ]]; then
        echo "Build Done"
    else
        echo "Copy Failed"
        exit 2
    fi

else
    echo "Build Failed"
    exit 3
fi
