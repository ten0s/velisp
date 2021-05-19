(defun vl-string->list (str)
  ; TODO: assert str is STR
  (if (zerop (strlen str)) nil
    (cons (ascii (substr str 1 1))
          (vl-string->list (substr str 2)))))

(defun vl-list->string (char-codes)
  ; TODO: assert char-codes is LIST
  (apply 'strcat (mapcar 'chr char-codes)))

(defun vl-string-left-trim (char-set str / aux)
  ; TODO: assert char-set is STR
  ; TODO: assert str is STR
  (defun aux (set lst / head tail)
    (setq head (car lst)
          tail (cdr lst))
    (cond ((null lst) lst)
          ((member head set) (aux set tail))
          (t lst)))
  (vl-list->string
   (aux (vl-string->list char-set)
        (vl-string->list str))))

(defun vl-string-right-trim (char-set str / aux)
  ; TODO: assert char-set is STR
  ; TODO: assert str is STR
  (defun aux (set lst / head tail)
    (setq head (car lst)
          tail (cdr lst))
    (cond ((null lst) lst)
          ((member head set) (aux set tail))
          (t lst)))
  (vl-list->string
   (reverse (aux (vl-string->list char-set)
                 (reverse (vl-string->list str))))))

(defun vl-string-trim (char-set str)
  ; TODO: assert char-set is STR
  ; TODO: assert str is STR
  (vl-string-left-trim
   char-set
   (vl-string-right-trim char-set str)))
