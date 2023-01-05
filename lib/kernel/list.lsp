;;;; SPDX-License-Identifier: 0BSD

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

(defun length (lst / len)
  (setq len 0)
  (while lst
    (setq len (1+ len)
          lst (cdr lst)))
  len)

(defun reverse (lst / rev)
  (foreach x lst
           (setq rev (cons x rev))))

(defun last (lst)
  (foreach x lst x))

(defun nth (n lst / x)
  (setq x (car lst))
  (while (and (/= n 0) lst)
    (setq lst (cdr lst)
          x (car lst)
          n (1- n)))
  x)

(defun member (val lst / break)
  (while (and (not break) lst)
    (if (not (setq break (equal val (car lst))))
        (setq lst (cdr lst))))
  lst)

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

(defun acad_strlsort (lst)
  (sort '< lst))
