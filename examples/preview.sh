#!/bin/bash

[[ ${DEBUG} != "" ]] && opts[k++]=--inspect-brk

if [[ $# -ne 1 ]]; then
    echo "Usage: preview.sh <NAME>"
    exit 1
fi

name=${1}

node ${opts[@]} src/main.js <<EOF
  (setq dcl_file "examples/$name.dcl")
  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
    (progn
      (princ (strcat "Error: dcl file '" dcl_file "' not loaded"))
      (exit 1)))
  (setq dlg_id "$name")
  (if (not (new_dialog dlg_id dcl_id))
    (progn
      (princ (strcat "Error: dialog '" dlg_id "' not found"))
      (exit 1)))
  (start_dialog)
  (unload_dialog dcl_id)
EOF
