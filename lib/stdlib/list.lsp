;;
;;
;;

;; > (caar (list (list 1 2 3) (list 4 5 6)))
;; 1
(defun caar (lst)
  ;; TODO: check length is at least 2
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

;;;
;;;
;;;

(defun caaar (lst)
  (car (caar lst)))

(defun caadr (lst)
  (car (cadr lst)))

(defun cadar (lst)
  (car (cdar list)))

;; > (setq pt (list 1.5 3.2 2.0))
;; > (setq z (cadr pt))
;; 2.0
(defun caddr (lst)
  (car (cddr lst)))

(defun cdaar (lst)
  (cdr (caar lst)))

(defun cdadr (lst)
  (cdr (cadr lst)))

(defun cddar (lst)
  (cdr (cddr lst)))

(defun cdddr (lst)
  (cdr (cddr lst)))

;;;;
;;;;
;;;;

(defun caaaar (lst)
  (car (caaar lst)))

(defun caaadr (lst)
  (car (caadr list)))

(defun caadar (lst)
  (car (cadar list)))

(defun caaddr (lst)
  (car (caddr list)))

(defun cadaar (lst)
  (car (cdaar lst)))

(defun cadadr (lst)
  (car (cdadr lst)))

(defun caddar (lst)
  (car (cddar lst)))

(defun cadddr (lst)
  (car (cdddr lst)))

(defun cdaaar (lst)
  (cdr (caaar lst)))

(defun cdaadr (lst)
  (cdr (caadr list)))

(defun cdadar (lst)
  (cdr (cadar list)))

(defun cdaddr (lst)
  (cdr (caddr list)))

(defun cddaar (lst)
  (cdr (cdaar lst)))

(defun cddadr (lst)
  (cdr (cdadr lst)))

(defun cdddar (lst)
  (cdr (cddar lst)))

(defun cddddr (lst)
  (cdr (cdddr lst)))

;; > (length (list))
;; 0
;; > (length (list 1))
;; 1
;; > (length (list 1 2 3))
;; 3
;; > (length (list (list 1) (list 2) (list 3)))
;; 3
(defun length (lst)
  ;; TODO: check type is List
  (if (null lst) 0
    (+ 1 (length (cdr lst)))))

;; > (reverse (list (list 'a) 'b 'c))
;; ('C 'B ('A))
(defun reverse (lst)
  (defun aux (lst acc)
    (cond ((null lst) acc)
          (t (aux (cdr lst) (cons (car lst) acc)))))
  (aux lst (list)))

;; > (last (list 'a 'b 'c 'd 'e))
;; 'E
;; > (last (list 'a 'b 'c (list 'd 'e)))
;; ('D 'E)
(defun last (lst)
  (defun aux (lst last)
    (cond ((null lst) last)
          (t (aux (cdr lst) (car lst)))))
  (aux (cdr lst) (car lst)))

;; > (nth 3 (list 'a 'b 'c 'd 'e))
;; 'D
;; > (nth 0 (list 'a 'b 'c 'd 'e))
;; 'A
;; > (nth 5 (list 'a 'b 'c 'd 'e))
;; nil
(defun nth (n lst)
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (nth (1- n) (cdr lst)))))

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
