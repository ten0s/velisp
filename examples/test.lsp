(setq dcl_file "test.dcl")
(setq dlg_id "ok_dlg")

(if (< (setq dcl_id (load_dialog dcl_file)) 0)
  (progn
    (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
    (exit 1)))

(if (not (new_dialog dlg_id dcl_id))
  (progn
    (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
    (exit 1)))

(action_tile "accept" "(done_dialog 1)")

(setq ret (start_dialog))
(princ (strcat "dialog done result: " (itoa ret) "\n"))
(unload_dialog dcl_id)
