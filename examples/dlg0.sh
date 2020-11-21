#!/bin/bash

[[ ${DEBUG} != "" ]] && opts[k++]=--inspect-brk

node ${opts[@]} src/main.js <<EOF
  (setq dcl_file "examples/dlg0.dcl")
  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
    (progn
      (princ (strcat "Error: dcl file '" dcl_file "' not loaded"))
      (exit 1)))
  (setq dlg_id "dlg0")
  (if (not (new_dialog dlg_id dcl_id))
    (progn
      (princ (strcat "Error: dialog '" dlg_id "' not found"))
      (exit 1)))
  ; TODO: remove once default action in dcl is implemented
  (action_tile "accept" "(done_dialog)")
  (setq ret (start_dialog))
  (princ (strcat "dialog done w/ " (itoa ret)))
  (unload_dialog dcl_id)
EOF
