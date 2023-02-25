;;;; SPDX-License-Identifier: 0BSD

(load "util.lsp")

;;; Define sleep function if not defined
(if (not sleep)
    (defun sleep (millisecs)
      (command "_.DELAY" millisecs)))

(setq SHELL_RUN_ONLY nil
      SHELL_RETURN_LINES T)

(defun shell (cmd return / os shellargs shellsep tmp_out tmp_done)
  ;; Run a command inside OS dependent shell
  ;; Usage:
  ;; > (shell "echo Hello" nil) ; run command, return nil
  ;; > (shell "echo Hello" T)   ; run command, return output lines
  (setq os (get_os))
  (defun shellargs ()
    (cond ((or (= os "Linux") (= os "MacOS"))
           (list (getenv "SHELL")   "-c"))
          ((= os "Windows")
           (list (getenv "COMSPEC") "/c"))
          (T
           (princ (strcat "No SHELL ARGS known for " os) "\n")
           '("echo"))))
  (defun shellsep ()
    (cond ((or (= os "Linux") (= os "MacOS"))
           ";")
          ((= os "Windows")
           "&")
          (T
           (princ (strcat "No SHELL SEP known for " os) "\n")
           ";")))
  (if return
      (setq tmp_out  (vl-filename-mktemp)
            tmp_done (vl-filename-mktemp)
            cmd (strcat "( " cmd " ) > " tmp_out " 2>&1"
                        " " (shellsep) " "
                        "echo DONE > " tmp_done)))
  (if (is_autocad)
      (command "_.SHELL" cmd)
    (apply 'startapp (append (shellargs) (list cmd))))
  (if return
      (progn
        (while (not (read_lines tmp_done)) (sleep 100))
        (read_lines tmp_out))))
