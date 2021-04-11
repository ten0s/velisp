(vlu-add-test
 (defun vl-string->list-test ()
   (vlu-assert-equal '() (vl-string->list ""))
   (vlu-assert-equal '(49) (vl-string->list "1"))
   (vlu-assert-equal '(49 50) (vl-string->list "12"))))

(vlu-add-test
 (defun vl-list->string-test ()
   (vlu-assert-equal "" (vl-list->string '()))
   (vlu-assert-equal "1" (vl-list->string '(49)))
   (vlu-assert-equal "12" (vl-list->string '(49 50)))))
