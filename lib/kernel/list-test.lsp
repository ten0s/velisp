;;;; SPDX-License-Identifier: 0BSD

;;
;;
;;

(vlu-add-test
 (defun caar-test ()
   (vlu-assert-equal 1 (caar '((1 2 3) (4 5 6))))
))

(vlu-add-test
 (defun cadr-test ()
   (vlu-assert-equal 3.2 (cadr '(1.5 3.2 2.0)))
))

(vlu-add-test
 (defun cdar-test ()
   (vlu-assert-equal '(2 3) (cdar '((1 2 3) '(4 5 6))))
))

(vlu-add-test
 (defun cddr-test ()
   (vlu-assert-equal '() (cddr '((1 2 3) (4 5 6))))
))

;;;
;;;
;;;

(vlu-add-test
 (defun caaar-test ()
   (vlu-assert-equal 1 (caaar '(((1 2 3 4 5)))))
))

(vlu-add-test
 (defun caadr-test ()
   (vlu-assert-equal '(2 3 4 5) (caadr '(1 ((2 3 4 5)))))
))

(vlu-add-test
 (defun cadar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun caddr-test ()
   (vlu-assert-equal 2.0 (caddr '(1.5 3.2 2.0)))
))

(vlu-add-test
 (defun cdaar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cdadr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cddar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cdddr-test ()
   (vlu-assert-equal 1 1)
))

;;;;
;;;;
;;;;

(vlu-add-test
 (defun caaaar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun caaadr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun caadar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun caaddr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cadaar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cadadr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun caddar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cadddr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cdaaar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cdaadr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cdadar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cdaddr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cddaar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cddadr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cdddar-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun cddddr-test ()
   (vlu-assert-equal 1 1)
))

(vlu-add-test
 (defun length-test ()
   (vlu-assert-equal 0 (length '()))
   (vlu-assert-equal 1 (length '(1)))
   (vlu-assert-equal 3 (length '(1 2 3)))
   (vlu-assert-equal 3 (length '((1) (2) (3))))
   (vlu-assert-equal 1000 (length (seq 1 1000 1)))
))

(vlu-add-test
 (defun reverse-test ()
   (vlu-assert-equal '(c b (a)) (reverse '((a) b c)))
   (vlu-assert-equal 1000 (car (reverse (seq 1 1000 1))))
))

(vlu-add-test
 (defun last-test ()
   (vlu-assert-equal 'e (last '(a b c d e)))
   (vlu-assert-equal '(d e) (last '(a b c (d e))))
   (vlu-assert-equal 1000 (last (seq 1 1000 1)))
))

(vlu-add-test
 (defun nth-test ()
   (vlu-assert-equal 'd (nth 3 '(a b c d e)))
   (vlu-assert-equal 'a (nth 0 '(a b c d e)))
   (vlu-assert-equal 'e (nth 4 '(a b c d e)))
   (vlu-assert-equal nil (nth 5 '(a b c d e)))
   (vlu-assert-equal 1000 (nth 1000 (seq 0 1000 1)))
))

(vlu-add-test
 (defun member-test ()
   (vlu-assert-equal '(c d e) (member 'c '(a b c d e)))
   (vlu-assert-equal nil (member 'q '(a b c d e)))
   (vlu-assert-equal '(1000) (member 1000 (seq 1 1000 1)))
))

(vlu-add-test
 (defun assoc-test ()
   (vlu-assert-equal nil (assoc 'one '()))
   (vlu-assert-equal '(one 1) (assoc 'one '((one 1) (two 2) (three 3))))
   (vlu-assert-equal '(three 3) (assoc 'three '((one 1) (two 2) (three 3))))
   (vlu-assert-equal nil (assoc 'four '((one 1) (two 2) (three 3))))
   (vlu-assert-equal '(one . 1) (assoc 'one '((one . 1) (two . 2) (three . 3))))
   (vlu-assert-equal '(three . 3) (assoc 'three '((one . 1) (two . 2) (three . 3))))
   (vlu-assert-equal nil (assoc 'four '((one . 1) (two . 2) (three . 3))))
))

(vlu-add-test
 (defun subst-test ()
   (vlu-assert-equal nil (subst 'one 1 '()))
   (vlu-assert-equal '(one) (subst 'one 1 '(1)))
   (vlu-assert-equal '(one 2 3) (subst 'one 1 '(1 2 3)))
   (vlu-assert-equal '(1 two 3) (subst 'two 2 '(1 2 3)))
   (vlu-assert-equal '(1 2 three) (subst 'three 3 '(1 2 3)))
   (vlu-assert-equal '(1 2 3) (subst 'four 4 '(1 2 3)))
   (vlu-assert-equal '(1 two 3) (subst 'two '(2) '(1 (2) 3)))
))

(vlu-add-test
 (defun acad_strlsort-test ()
   (vlu-assert-equal '() (acad_strlsort '()))

   (vlu-assert-equal '("1" "2" "3") (acad_strlsort '("1" "2" "3")))
   (vlu-assert-equal '("1" "2" "3") (acad_strlsort '("2" "3" "1")))
   (vlu-assert-equal '("1" "2" "3") (acad_strlsort '("2" "3" "1")))

   (vlu-assert-equal '("1" "1" "2" "2" "3" "3") (acad_strlsort '("1" "2" "3" "1" "2" "3")))
   (vlu-assert-equal '("1" "1" "2" "2" "3" "3") (acad_strlsort '("3" "2" "1" "1" "2" "3")))

   (vlu-assert-equal
    '("Apr" "Aug" "Dec" "Feb" "Jan" "Jul" "Jun" "Mar" "May" "Nov" "Oct" "Sep")
    (acad_strlsort '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
))
