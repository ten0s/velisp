#!/usr/bin/env bash

export VAGRANT_VAGRANTFILE=Vagrantfile.macos.build

export MACOS_NAME=catalina


vagrant up --provision
if [[ $? -ne 0 ]]; then
    echo "Build Failed"
    exit 1
fi

vagrant ssh -c 'source ~/.zshrc; cd velisp; macos/update.sh'
if [[ $? -ne 0 ]]; then
    echo "Update Failed."
    exit 2
fi

vagrant ssh -c 'source ~/.zshrc; cd velisp; macos/build.sh'
if [[ $? -eq 0 ]]; then
    # Since 'rsync' synced_folder supported for MacOS guests is
    # host to guest only, rsync the file back explicitly
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
        exit 3
    fi

else
    echo "Build Failed"
    exit 4
fi
