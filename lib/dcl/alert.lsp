(defun alert (message / dlg_id dcl_id dcl_file file)
  (setq dlg_id "alert_dlg")
  (setq dcl_file (vl-filename-mktemp (strcat dlg_id "-") "" ".dcl"))
  (setq file (open dcl_file "w"))

  (princ  (strcat dlg_id " : dialog {          " EOL) file)
  (princ  (strcat "  label = \"Alert Message\";" EOL) file)
  (princ  (strcat "  : paragraph {             " EOL) file)
  (princ  (strcat "    alignment = centered;   " EOL) file)

  (foreach line (split EOL message)
           (princ  (strcat "    : text_part  {         " EOL) file)
           (princ  (strcat "      label = \"" line "\";" EOL) file)
           (princ  (strcat "      alignment = centered;" EOL) file)
           (princ  (strcat "    }                      " EOL) file))

  (princ  (strcat "  }                           " EOL) file)
  (princ  (strcat "  ok_only;                    " EOL) file)
  (princ  (strcat "}                             " EOL) file)

  (close file)

  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
    (progn
      (princ (strcat "Error: dcl file '" dcl_file "' not loaded"))
      (exit 1)))

  (if (not (new_dialog dlg_id dcl_id))
    (progn
      (princ (strcat "Error: dialog '" dlg_id "' not found"))
      (exit 1)))

  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete dcl_file)
  nil)
