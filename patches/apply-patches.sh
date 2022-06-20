#!/bin/bash

DIR=$(dirname $0)

if [[ ! -f ${DIR}/../node_modules/node-gtk/lib/overrides/Gtk-3.0.js.orig ]]; then
    patch --backup ${DIR}/../node_modules/node-gtk/lib/overrides/Gtk-3.0.js ${DIR}/Gtk-3.0.js.patch
fi

if [[ `uname -s` =~ "MINGW64" ]]; then

# Don't free array memory. On Windows there are unknown crashes.
# Needs to be investigated.
if [[ ! -f ${DIR}/../node_modules/node-gtk/src/value.cc.orig ]]; then
    patch --backup ${DIR}/../node_modules/node-gtk/src/value.cc ${DIR}/value.cc.patch
fi

fi
