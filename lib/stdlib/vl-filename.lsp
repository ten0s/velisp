(defun vl-filename-base (filename)
  (cdr (assoc 'name (ve-filename-parse filename))))

(defun vl-filename-directory (filename)
  (cdr (assoc 'dir (ve-filename-parse filename))))

(defun vl-filename-extension (filename / ext)
  (setq ext (cdr (assoc 'ext (ve-filename-parse filename))))
  (if (eq ext "") nil ext))
