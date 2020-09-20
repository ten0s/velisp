;; > (caar (list (list 1 2 3) (list 4 5 6)))
;; 1
(defun caar (lst)
  (car (car lst)))

;; > (setq pt (list 1.5 3.2 2.0))
;; > (setq y (cadr pt))
;; 3.2
(defun cadr (lst)
  (car (cdr lst)))

;; > (cdar (list (list 1 2 3) (list 4 5 6)))
;; (2 3)
(defun cdar (lst)
  (cdr (car lst)))

;; > (cddr (list (list 1 2 3) (list 4 5 6)))
;; ()
(defun cddr (lst)
  (cdr (cdr lst)))

;; > (setq pt (list 1.5 3.2 2.0))
;; > (setq z (cadr pt))
;; 2.0
(defun caddr (lst)
  (car (cdr (cdr lst))))

;; TODO
;; caaar cadar cdaar cddar
;; caadr cdadr cdddr
;; caaadr cadadr cdaadr cddadr
;; caadar caddar cdadar cdddar
;; caaaar cadaar cdaaar cddaar
;; caaddr cadddr cdaddr cddddr

;; > (member 'c (list 'a 'b 'c 'd 'e))
;; ('C 'D 'E)
;; > (member 'q (list 'a 'b 'c 'd 'e))
;; nil
(defun member (val lst)
  (if (null lst) nil
    (if (equal val (car lst)) lst
      (member val (cdr lst)))))

;; > (mapcar '1+ (list))
;; nil
;; > (mapcar '1+ (list 1 2 3))
;; (2 3 4)
(defun mapcar (fn lst)
  (if (null lst) nil
    (cons (fn (car lst))
          (mapcar fn (cdr lst)))))
