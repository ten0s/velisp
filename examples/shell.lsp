;;;; SPDX-License-Identifier: 0BSD

(load "util.lsp")

(setq SHELL_RUN_ONLY nil
      SHELL_RETURN_LINES T)

(defun shell (cmd return / os shellargs shellsep tmp_out tmp_done tmp_bat file)
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
      (setq tmp_out  (vl-filename-mktemp "out.txt")
            tmp_done (vl-filename-mktemp "done.txt")
            cmd (strcat "( " cmd " ) > " tmp_out " 2>&1"
                        " " (shellsep) " "
                        "echo DONE > " tmp_done)))
  (if (is_autocad)
    (progn
      ;; (command "_.SHELL" cmd) hangs inside DCL
      (setq tmp_bat (vl-filename-mktemp "run.bat")
            file (open tmp_bat "w"))
      (write-line "@echo off" file)
      (write-line cmd file)
      (close file)
      (startapp tmp_bat))
    (apply 'startapp (append (shellargs) (list cmd))))
  (if return
      (progn
        (while (not (read_lines tmp_done))
          (if (not (is_autocad))
              ;; (command "_.DELAY" 100) hangs inside DCL
              (sleep 100)))
        (read_lines tmp_out))))
