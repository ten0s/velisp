;;;; SPDX-License-Identifier: 0BSD

(defun is_autocad ()
  ;; Determine if running under AutoCAD
  (not (getvar "VELISP-VERSION")))

(defun get_os ()
  ;; Determine OS
  (if (is_autocad) "Windows"
    (nth 3 (split " " (ver)))))

(defun path_sep ()
  (if (= (get_os) "Windows") "\\" "/"))

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

(defun with_dialog (dcl_file dlg_id action init_fun done_fun / dcl_id ret)
  (if (< (setq dcl_id (load_dialog dcl_file)) 0)
    (progn
      (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
      (exit 1)))

  (if (not (new_dialog dlg_id dcl_id action))
    (progn
      (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
      (exit 1)))

  (if (not done_fun)
      (setq done_fun (lambda (x) x)))

  (init_fun)
  (setq ret (done_fun (start_dialog)))

  (unload_dialog dcl_id)
  ret)
