;; VeLisp Extension
(defun join (delim lst / acc)
  (if (= (length lst) 1) (setq acc lst)
    (progn
      (setq acc '())
      (foreach item lst
               ;; Add item and then delim
               (setq acc (cons delim (cons item acc))))
      ;; Remove last delim and reverse
      (setq acc (reverse (cdr acc)))))
  (apply 'strcat
         (mapcar 'vl-princ-to-string acc)))
