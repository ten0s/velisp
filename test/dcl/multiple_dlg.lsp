(defun new_dlg (dcl_file dlg_id / dcl_id ret)
  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
      (progn
        (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
        (exit 1)))

  (if (not (new_dialog dlg_id dcl_id))
      (progn
        (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
        (exit 1)))

  (action_tile "new_dlg" "(new_dlg dcl_file dlg_id)")
  (action_tile "term_all" "(term_dialog)")

  (setq ret (start_dialog))
  (princ (strcat "dialog done w/ " (itoa ret) "\n"))
  (unload_dialog dcl_id)
)

(action_tile "new_dlg" "(new_dlg \"test/dcl/multiple_dlg.dcl\" \"multiple_dlg\")")
(action_tile "term_all" "(term_dialog)")
