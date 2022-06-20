#!/bin/bash -e

cd node_modules/node-gtk
npx node-pre-gyp configure build --debug
cd ../..
