;; VeLisp Extension
(defun split (delim str / delim-len do-split)
  ;; Split a string using a delimiter
  (setq delim-len (strlen delim))
  (defun do-split (str / pos)
    (if (setq pos (vl-string-search delim str))
        (cons (substr str 1 pos)
              (do-split (substr str (+ pos 1 delim-len))))
      (list str)))
  (if (zerop delim-len)
      (mapcar 'chr (vl-string->list str))
    (do-split str)))
