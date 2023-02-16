#!/bin/bash -e

curl -s https://raw.githubusercontent.com/ten0s/node-gtk/master/windows/mingw_include_extra.sh | bash

export MINGW_WINDOWS_PATH=$(curl -s https://raw.githubusercontent.com/ten0s/node-gtk/master/windows/mingw_windows_path.sh | bash)

# Install deps
TEMP=$(mktemp -d)
pushd $TEMP
git clone https://github.com/ten0s/msys2-velisp $PWD
make && make install
popd
rm -rf $TEMP

make windowsPackage

cp velisp-*-*-*.{exe,zip} /c/vagrant/
