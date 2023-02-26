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

(defun filter (fun lst / acc)
  (foreach elm lst
           (if (fun elm)
               (setq acc (cons elm acc))))
  (reverse acc))

(defun duplicate (n elm / acc)
  (while (> n 0)
    (setq n (1- n)
          acc (cons elm acc))))

(defun evenp (num)
  (and (= (type num) 'INT)
       (= (rem num 2) 0)))

(defun oddp (num)
  (and (= (type num) 'INT)
       (/= (rem num 2) 0)))

(defun println (what)
  (if (atom what)
      (princ what)
    (foreach str what (princ str)))
  (princ "\n")
  what)

(defun inspect (msg what)
  (princ msg)
  (println what))
