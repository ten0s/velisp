(setq TILE_MODE_ENABLE 0)
(setq TILE_MODE_DISABLE 1)
(setq TILE_MODE_FOCUS 2)
(setq TILE_MODE_SELECT_EDITBOX 3)
(setq TILE_MODE_FLIP_IMAGE_HIGHLIGHT 4)

(setq START_LIST_CHANGE 1)
(setq START_LIST_APPEND 2)
(setq START_LIST_CLEAR 3)

(setq ACTION_REASON_SELECTED 1)
(setq ACTION_REASON_FOCUS_LOST 2)
(setq ACTION_REASON_INTERIM_CHANGE 3)
(setq ACTION_REASON_DOUBLE_CLICK 4)

(defun alert (message / dlg_id dcl_id dcl_file file)
  (setq dlg_id "alert_dlg")
  (setq dcl_file (vl-filename-mktemp (strcat dlg_id "-") "" ".dcl"))
  (setq file (open dcl_file "w"))

  (princ  (strcat dlg_id " : dialog {          \n") file)
  (princ  "  label = \"Alert Message\";        \n"  file)
  (princ  "  : paragraph {                     \n"  file)
  (princ  "    alignment = centered;           \n"  file)

  (foreach line (ve-string-split "\n" message)
           (princ  "    : text_part  {                  \n"  file)
           (princ  (strcat "      label = \"" line "\"; \n") file)
           (princ  "      alignment = centered;         \n"  file)
           (princ  "    }                               \n"  file))

  (princ  "  }                                 \n"  file)
  (princ  "  ok_only;                          \n"  file)
  (princ  "}                                   \n"  file)

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
