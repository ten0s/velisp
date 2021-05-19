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

(vlu-add-test
 (defun vl-string-left-trim-test ()
   (vlu-assert-equal "\n\t STR "
                     (vl-string-left-trim "" "\n\t STR "))
   (vlu-assert-equal "STR "
                     (vl-string-left-trim " \t\n" "\n\t STR "))
   (vlu-assert-equal "3CPO is not R2D2"
                     (vl-string-left-trim "12456789" "12463CPO is not R2D2"))
   (vlu-assert-equal "Too many spaces "
                     (vl-string-left-trim " " "   Too many spaces "))))

(vlu-add-test
 (defun vl-string-right-trim-test ()
   (vlu-assert-equal " STR \n\t "
                     (vl-string-right-trim "" " STR \n\t "))
   (vlu-assert-equal " STR"
                     (vl-string-right-trim " \t\n" " STR \n\t "))
   (vlu-assert-equal "3CPO is not R2D2"
                     (vl-string-right-trim "1356789" "3CPO is not R2D267891"))
   (vlu-assert-equal " Too many spaces"
                     (vl-string-right-trim " " " Too many spaces  "))))

(vlu-add-test
 (defun vl-string-trim-test ()
   (vlu-assert-equal " \t\n STR \n\t "
                     (vl-string-trim "" " \t\n STR \n\t "))
   (vlu-assert-equal "STR"
                     (vl-string-trim " \t\n" " \t\n STR \n\t "))
   (vlu-assert-equal "Don't call this junk!"
                     (vl-string-trim "this is junk" "this is junk Don't call this junk! this is junk"))
   (vlu-assert-equal "Too many spaces"
                     (vl-string-trim " " "   Too many spaces  "))))
