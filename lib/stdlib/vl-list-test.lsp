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
