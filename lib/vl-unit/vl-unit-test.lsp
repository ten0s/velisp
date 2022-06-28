;;;; SPDX-License-Identifier: 0BSD

(vlu-add-test
 (defun add-tests ()
   (vlu-assert-equal 3 (+ 1 2))
   (vlu-assert-equal 6 (+ (+ 1 2) 3))
   (vlu-assert-equal 0 0)))

(vlu-add-test
 (defun equal-tests ()
   (vlu-assert       (equal nil nil))
   (vlu-assert-not   (equal nil t))
   (vlu-assert-equal nil nil)
   (vlu-assert-equal '(a) '(a))))

(vlu-add-test
 (defun true-tests ()
   (vlu-assert t)
   (vlu-assert 0)
   (vlu-assert 1)
   (vlu-assert 'one)
   (vlu-assert '(a))))

(vlu-add-test
 (defun false-tests ()
   (vlu-assert-not nil)))
