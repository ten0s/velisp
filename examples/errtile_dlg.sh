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

  (defun getint ( / str1 str2 int)
    (setq str1 (get_tile "edit"))
    (setq int (atoi str1))
    (setq str2 (itoa int))
    (if (and (>= int 0) (= str1 str2))
      (progn
        (set_tile "error" "")
        (set_tile "text" (itoa int)))
      (progn
        (set_tile "error" "Invalid Integer")
        (set_tile "text" "")
        (mode_tile "edit" 2))))

  (action_tile "getint" "(getint)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq ret (start_dialog))
  (princ (strcat "dialog done: " (itoa ret)))
  (unload_dialog dcl_id)
EOF
