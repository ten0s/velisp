(vlu-add-test
 (defun vl-member-if-test ()
   (vlu-assert-equal '() (vl-member-if '(lambda (x) t) '()))
   (vlu-assert-equal '() (vl-member-if '(lambda (x) (< x 0)) '(1 2 3)))
   (vlu-assert-equal '(-2 3) (vl-member-if '(lambda (x) (< x 0)) '(1 -2 3)))
))

(vlu-add-test
 (defun vl-member-if-not-test ()
   (vlu-assert-equal '() (vl-member-if-not '(lambda (x) t) '()))
   (vlu-assert-equal '(1 2 3) (vl-member-if-not '(lambda (x) (< x 0)) '(1 2 3)))
   (vlu-assert-equal '(0 1) (vl-member-if-not '(lambda (x) (< x 0)) '(-1 0 1)))
))

(vlu-add-test
 (defun vl-position-test ()
   (vlu-assert-equal nil (vl-position 0 '()))
   (vlu-assert-equal nil (vl-position 0 '(1 2 3)))
   (vlu-assert-equal 0 (vl-position 1 '(1 2 3)))
   (vlu-assert-equal 2 (vl-position 3 '(1 2 3)))
   (vlu-assert-equal 1 (vl-position "c" '("a" "c" "b")))
   (vlu-assert-equal 1 (vl-position 'b '(a b c)))
))

(vlu-add-test
 (defun vl-remove-test ()
   (vlu-assert-equal nil (vl-remove 1 '()))
   (vlu-assert-equal '(2 3 4) (vl-remove 1 '(1 2 3 4 1)))
   (vlu-assert-equal '(2 3) (vl-remove 1 '(1 2 3 1)))
   (vlu-assert-equal '(1 3 1) (vl-remove 2 '(1 2 3 1)))
   (vlu-assert-equal '(1 2 3 1) (vl-remove 4 '(1 2 3 1)))
))

(vlu-add-test
 (defun vl-remove-if-test ()
   (vlu-assert-equal '() (vl-remove-if '(lambda (x) t) '()))
   (vlu-assert-equal '() (vl-remove-if '(lambda (x) t) '(1 2 3)))
   (vlu-assert-equal '(1 2 3) (vl-remove-if '(lambda (x) nil) '(1 2 3)))
   (vlu-assert-equal '(1 2 3) (vl-remove-if '(lambda (x) (< x 0)) '(1 2 3)))
   (vlu-assert-equal '(1 3) (vl-remove-if '(lambda (x) (< x 0)) '(1 -2 3 -3)))
))

(vlu-add-test
 (defun vl-remove-if-not-test ()
   (vlu-assert-equal '() (vl-remove-if-not '(lambda (x) t) '()))
   (vlu-assert-equal '(1 2 3) (vl-remove-if-not '(lambda (x) t) '(1 2 3)))
   (vlu-assert-equal '() (vl-remove-if-not '(lambda (x) nil) '(1 2 3)))
   (vlu-assert-equal '() (vl-remove-if-not '(lambda (x) (< x 0)) '(1 2 3)))
   (vlu-assert-equal '(-2 -3) (vl-remove-if-not '(lambda (x) (< x 0)) '(1 -2 3 -3)))
))

(vlu-add-test
 (defun vl-sort-test ()
   (vlu-assert-equal '() (vl-sort '() '<))

   (vlu-assert-equal '(1 2 3) (vl-sort '(1 2 3) '<))
   (vlu-assert-equal '(3 2 1) (vl-sort '(1 2 3) '>))

   (vlu-assert-equal '(1 1 2 2 3 3) (vl-sort '(1 1 2 2 3 3) '<))
   (vlu-assert-equal '(3 3 2 2 1 1) (vl-sort '(1 1 2 2 3 3) '>))

   (vlu-assert-equal '(1 2 3 4 5) (vl-sort '(2 3 1 5 4) '<))

   (vlu-assert-equal '("1" "2" "3") (vl-sort '("2" "3" "1") '<))
   (vlu-assert-equal '("3" "2" "1") (vl-sort '("2" "3" "1") '>))

   (vlu-assert-equal '((3 1) (2 2) (1 3))
                     (vl-sort '((1 3) (2 2) (3 1))
                              '(lambda (e1 e2)
                                 (< (cadr e1) (cadr e2)))))

   (vlu-assert-equal '(a a b c d) ; Note that both As remain in the result list
                     (vl-sort
                      '(a d c b a)
                      '(lambda (s1 s2)
                         (< (vl-symbol-name s1) (vl-symbol-name s2)))))
))

(vlu-add-test
 (defun vl-sort-i-test ()
   (vlu-assert-equal '() (vl-sort-i '() '<))

   (vlu-assert-equal '(0 1 2) (vl-sort-i '(1 2 3) '<))
   (vlu-assert-equal '(2 1 0) (vl-sort-i '(1 2 3) '>))

   (vlu-assert-equal '(1 0 3 2 5 4) (vl-sort-i '(1 1 2 2 3 3) '<))
   (vlu-assert-equal '(5 4 3 2 1 0) (vl-sort-i '(1 1 2 2 3 3) '>))

   (vlu-assert-equal '(2 0 1 4 3) (vl-sort-i '(2 3 1 5 4) '<))

   (vlu-assert-equal '(2 0 1) (vl-sort-i '("2" "3" "1") '<))
   (vlu-assert-equal '(1 0 2) (vl-sort-i '("2" "3" "1") '>))

   (vlu-assert-equal '(2 1 0)
                     (vl-sort-i '((1 3) (2 2) (3 1))
                              '(lambda (e1 e2)
                                 (< (cadr e1) (cadr e2)))))

   (vlu-assert-equal '(4 0 3 2 1)
                     (vl-sort-i
                      '(a d c b a)
                      '(lambda (s1 s2)
                         (< (vl-symbol-name s1) (vl-symbol-name s2)))))
))
