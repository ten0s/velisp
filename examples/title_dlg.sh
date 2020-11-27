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

  (setq index 1)
  (setq prefix "Title")

  (defun title (prefix index)
    (strcat prefix " " (itoa index)))

  (defun change ( / old_title new_title)
    (setq old_title (get_tile "dialog"))
    (setq index (1+ index))
    (setq new_title (title prefix index))
    (set_tile "old_title" old_title)
    (set_tile "dialog" new_title))

  (set_tile "dialog" (title prefix index))
  (action_tile "change" "(change)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq ret (start_dialog))
  (princ (strcat "dialog done w/ " (itoa ret)))
  (unload_dialog dcl_id)
EOF
