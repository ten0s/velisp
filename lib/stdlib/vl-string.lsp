(defun vl-string->list (str)
  ; TODO: assert str is STR
  (if (zerop (strlen str)) nil
    (cons (ascii (substr str 1 1))
          (vl-string->list (substr str 2)))))

(defun vl-list->string (char-codes)
  ; TODO: assert char-codes is LIST
  (apply 'strcat (mapcar 'chr char-codes)))
