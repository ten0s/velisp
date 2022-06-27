#!/bin/bash -e

curl -s https://raw.githubusercontent.com/ten0s/node-gtk/windows-build/windows/mingw_include_extra.sh | bash

export MINGW_WINDOWS_PATH=$(curl -s https://raw.githubusercontent.com/ten0s/node-gtk/windows-build/windows/mingw_windows_path.sh | bash)

pacman --noconfirm -Syu

make windowsPackage

cp velisp-*-*-*.zip /c/vagrant/
