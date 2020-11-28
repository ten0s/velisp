#!/bin/bash

[[ ${DEBUG} != "" ]] && opts[k++]=--inspect-brk

base=$(basename $0)
name=${base%%.sh}

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

  (defun on_off (val)
     (if (= val "1") "on" "off"))

  (set_tile "toggle1-value" (on_off (get_tile "toggle1")))
  (set_tile "toggle2-value" (on_off (get_tile "toggle2")))
  (action_tile "toggle2" "(set_tile \"toggle2-value\" (on_off \$value))")

  (setq ret (start_dialog))
  (princ (strcat "dialog done w/ " (itoa ret)))
  (unload_dialog dcl_id)
EOF
