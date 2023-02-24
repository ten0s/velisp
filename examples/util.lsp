;;;; SPDX-License-Identifier: 0BSD

(defun is_autocad ()
  ;; Determine if running under AutoCAD
  (not (getvar "VELISP-VERSION")))

(defun get_os ()
  ;; Determine OS
  (if (is_autocad) "Windows"
    (nth 3 (split " " (ver)))))

(defun read_lines (file / fd line lines)
  ;; Read lines from file
  (if (setq fd (open file "r"))
      (progn
        (while (setq line (read-line fd))
          (setq lines (cons line lines)))
        (close fd)
        (reverse lines))))
