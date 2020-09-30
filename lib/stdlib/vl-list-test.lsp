(vlu-add-test
 (defun vl-remove-test ()
   (vlu-assert-equal nil (vl-remove 1 (list)))
   (vlu-assert-equal (list 2 3 4) (vl-remove 1 (list 1 2 3 4 1)))
   (vlu-assert-equal (list 2 3) (vl-remove 1 (list 1 2 3 1)))
   (vlu-assert-equal (list 1 3 1) (vl-remove 2 (list 1 2 3 1)))
   (vlu-assert-equal (list 1 2 3 1) (vl-remove 4 (list 1 2 3 1)))))
