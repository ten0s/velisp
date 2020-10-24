#!/bin/bash

[[ ${DEBUG} != "" ]] && opts[k++]=--inspect

node ${opts[@]} src/main.js <<EOF
  (setq dcl_file "examples/dlg1.dcl")
  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
    (progn
      (princ (strcat "Error: dcl file '" dcl_file "' not loaded"))
      (exit 1)))
  (setq dlg_id "dlg_2")
  (if (not (new_dialog dlg_id dcl_id))
    (progn
      (princ (strcat "Error: dialog '" dlg_id "' not found"))
      (exit 1)
     ))
  (new_dialog dlg_id dcl_id)
  (action_tile "accept" "(done_dialog)")
  (setq ret (start_dialog))
  (princ (strcat "dialog done w/ " (itoa ret)))
  (unload_dialog dcl_id)
EOF
