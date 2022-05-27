#!/bin/bash

DIR=$(dirname $0)

if [[ ! -f ${DIR}/../node_modules/node-gtk/lib/overrides/Gtk-3.0.js.orig ]]; then
    patch --backup ${DIR}/../node_modules/node-gtk/lib/overrides/Gtk-3.0.js ${DIR}/Gtk-3.0.js.patch
fi
