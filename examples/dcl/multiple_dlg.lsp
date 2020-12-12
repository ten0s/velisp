(defun new_dlg (dcl_file dlg_id / dcl_id ret)
  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
      (progn
        (princ (strcat "Error: dcl file '" dcl_file "' not loaded"))
        (exit 1)))

  (if (not (new_dialog dlg_id dcl_id))
      (progn
        (princ (strcat "Error: dialog '" dlg_id "' not found"))
        (exit 1)))

  (action_tile "newdlg" "(new_dlg dcl_file dlg_id)")

  (setq ret (start_dialog))
  (princ (strcat "dialog done w/ " (itoa ret)))
  (unload_dialog dcl_id)
)

(action_tile "newdlg" "(new_dlg \"examples/dcl/multiple_dlg.dcl\" \"multiple_dlg\")")
