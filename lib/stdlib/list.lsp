;; (member 'c (list 'a 'b 'c 'd 'e))
;; => ('C 'D 'E)
;; (member 'q (list 'a 'b 'c 'd 'e))
;; => nil
(defun member (val lst)
  (if (null lst) nil
    (if (equal val (car lst)) lst
      (member val (cdr lst)))))
