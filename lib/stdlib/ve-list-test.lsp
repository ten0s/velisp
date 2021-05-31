(vlu-add-test
 (defun join-test ()
   (vlu-assert-equal "" (join "" '()))
   (vlu-assert-equal "a" (join "" '("a")))
   (vlu-assert-equal "abc" (join "" '("a" "b" "c")))

   (vlu-assert-equal "" (join " " '("")))
   (vlu-assert-equal "a" (join " " '("a")))
   (vlu-assert-equal "a b c" (join " " '("a" "b" "c")))

   (vlu-assert-equal "" (join "," '("")))
   (vlu-assert-equal "," (join "," '("" "")))
   (vlu-assert-equal "1,2,3"(join "," '("1" "2" "3")))
   (vlu-assert-equal "1,2,3,," (join "," '("1" "2" "3" "" "")))
   (vlu-assert-equal "1,2,,3,," (join "," '("1" "2" "" "3" "" "")))

   (vlu-assert-equal "1,2,3" (join "," '(1 2 3)))
   (vlu-assert-equal "(1), (2 3), (4 5 nil)" (join ", " '((1) (2 3) (4 5 nil))))
))
