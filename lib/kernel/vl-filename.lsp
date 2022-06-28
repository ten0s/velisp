;;;; SPDX-License-Identifier: 0BSD

(defun vl-filename-base (filename)
  (cdr (assoc 'name (filename-parse filename))))

(defun vl-filename-directory (filename)
  (cdr (assoc 'dir (filename-parse filename))))

(defun vl-filename-extension (filename / ext)
  (setq ext (cdr (assoc 'ext (filename-parse filename))))
  (if (eq ext "") nil ext))
