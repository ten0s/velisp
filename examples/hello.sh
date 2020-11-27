#!/bin/bash

[[ ${DEBUG} != "" ]] && opts[k++]=--inspect-brk

node ${opts[@]} src/main.js examples/hello.lsp
