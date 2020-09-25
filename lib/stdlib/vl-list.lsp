(defun vl-remove (elm lst)
  (if (null lst) nil
    (if (equal elm (car lst)) (vl-remove elm (cdr lst))
      (cons (car lst)
            (vl-remove elm (cdr lst))))))
