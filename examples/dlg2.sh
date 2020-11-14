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
      (exit 1)))
;  (action_tile "edit1" "(set_tile \"text1\" (get_tile \"edit1\"))")
  (action_tile "edit1" "(set_tile \"text1\" \$value)")
  (action_tile "edit2" "(set_tile \"text2\" \$value)")
  (defun concat ()
    (strcat (get_tile "edit1") " " (get_tile "edit2")))
  (action_tile "accept" "(set_tile \"text1\" (concat)) (set_tile \"text2\" (concat))")
  (action_tile "cancel" "(done_dialog)")
  (setq ret (start_dialog))
  (princ (strcat "dialog done w/ " (itoa ret)))
  (unload_dialog dcl_id)
EOF
