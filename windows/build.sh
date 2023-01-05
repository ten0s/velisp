#!/bin/bash -e

curl -s https://raw.githubusercontent.com/ten0s/node-gtk/master/windows/mingw_include_extra.sh | bash

export MINGW_WINDOWS_PATH=$(curl -s https://raw.githubusercontent.com/ten0s/node-gtk/master/windows/mingw_windows_path.sh | bash)

make windowsPackage

cp velisp-*-*-*.{exe,zip} /c/vagrant/
