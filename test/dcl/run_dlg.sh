#!/bin/bash

[[ ${DEBUG} != "" ]] && opts[k++]=--inspect-brk

if [[ $# -ne 1 ]]; then
    echo "Usage: $(basename $0) <DCL_NAME>"
    exit 1
fi

dir=$(dirname $0)
base=$(basename $0)
name=$1

init=""
if [[ -f "${dir}/${name}.lsp" ]]; then
    init=$(< ${dir}/${name}.lsp)
fi

node ${opts[@]} ${dir}/../../src/main.js <<EOF
  (setq dcl_file "${dir}/${name}.dcl")
  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
    (progn
      (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
      (exit 1)))

  (defun default (key value reason)
    (princ (strcat "default: key: " key " value: " value " reason: " (itoa reason) "\n")))

  (setq dlg_id "${name}")
  (if (not (new_dialog dlg_id dcl_id "(default \$key \$value \$reason)" '(-1 -1)))
    (progn
      (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
      (exit 1)))

  ${init}

  (setq ret (start_dialog))
  (princ (strcat "dialog done w/ " (itoa ret) "\n"))
  (unload_dialog dcl_id)
EOF
