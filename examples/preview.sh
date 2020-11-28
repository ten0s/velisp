#!/bin/bash

[[ ${DEBUG} != "" ]] && opts[k++]=--inspect-brk

if [[ $# -lt 1 ]] || [[ $# -gt 2 ]] ; then
    echo "Usage: preview.sh <DCL_FILE> [DLG_ID]"
    exit 1
fi

dcl_file=${1}

if [[ ! -z ${2} ]]; then
    dlg_id=${2}
else
    dlg_id=${dcl_file##*/}
    dlg_id=${dlg_id%%.dcl}
fi

velisp <<EOF
  (setq dcl_file "$dcl_file")
  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
    (progn
      (princ (strcat "Error: dcl file '" dcl_file "' not loaded"))
      (exit 1)))
  (setq dlg_id "$dlg_id")
  (if (not (new_dialog dlg_id dcl_id))
    (progn
      (princ (strcat "Error: dialog '" dlg_id "' not found"))
      (exit 1)))
  (start_dialog)
  (unload_dialog dcl_id)
EOF
