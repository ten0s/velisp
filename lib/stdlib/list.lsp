;;
;;
;;

(defun caar (lst)
  ;; TODO: check length is at least 2
  (car (car lst)))

(defun cadr (lst)
  (car (cdr lst)))

(defun cdar (lst)
  (cdr (car lst)))

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
  (car (cdar lst)))

(defun caddr (lst)
  (car (cddr lst)))

(defun cdaar (lst)
  (cdr (caar lst)))

(defun cdadr (lst)
  (cdr (cadr lst)))

(defun cddar (lst)
  (cdr (cdar lst)))

(defun cdddr (lst)
  (cdr (cddr lst)))

;;;;
;;;;
;;;;

(defun caaaar (lst)
  (car (caaar lst)))

(defun caaadr (lst)
  (car (caadr lst)))

(defun caadar (lst)
  (car (cadar lst)))

(defun caaddr (lst)
  (car (caddr lst)))

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
  (cdr (caadr lst)))

(defun cdadar (lst)
  (cdr (cadar lst)))

(defun cdaddr (lst)
  (cdr (caddr lst)))

(defun cddaar (lst)
  (cdr (cdaar lst)))

(defun cddadr (lst)
  (cdr (cdadr lst)))

(defun cdddar (lst)
  (cdr (cddar lst)))

(defun cddddr (lst)
  (cdr (cdddr lst)))

(defun length (lst)
  ;; TODO: check type is List
  (if (null lst) 0
    (+ 1 (length (cdr lst)))))

(defun reverse (lst / aux)
  (defun aux (lst acc)
    (cond ((null lst) acc)
          (t (aux (cdr lst) (cons (car lst) acc)))))
  (aux lst (list)))

(defun last (lst / aux)
  (defun aux (lst last)
    (cond ((null lst) last)
          (t (aux (cdr lst) (car lst)))))
  (aux (cdr lst) (car lst)))

(defun nth (n lst)
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (nth (1- n) (cdr lst)))))

(defun member (val lst)
  (if (null lst) nil
    (if (equal val (car lst)) lst
      (member val (cdr lst)))))

(defun assoc (elm alst)
  (cond ((null alst) nil)
        ((equal elm (caar alst)) (car alst))
        (t (assoc elm (cdr alst)))))

(defun subst (newitem olditem lst)
  (cond ((null lst) nil)
        ((equal (car lst) olditem)
         (cons newitem
               (subst newitem olditem (cdr lst))))
        (t
         (cons (car lst)
               (subst newitem olditem (cdr lst))))))
