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

  (defun toggle ( / btn_label)
     (setq label (get_tile "toggle"))
     (cond ((= label "Disable")
              (mode_tile "edit1" 1)
              (set_tile "toggle" "Enable"))
           ((= label "Enable")
              (mode_tile "edit1" 0)
              (set_tile "toggle" "Focus"))
           ((= label "Focus")
              (mode_tile "edit1" 2)
              (set_tile "toggle" "Disable"))))

  (action_tile "toggle" "(toggle)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq ret (start_dialog))
  (princ (strcat "dialog done: " (itoa ret)))
  (unload_dialog dcl_id)
EOF
