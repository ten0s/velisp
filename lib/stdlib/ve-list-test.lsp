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

(vlu-add-test
 (defun sort-test ()
   (vlu-assert-equal '() (sort '< '()))

   (vlu-assert-equal '(1 2 3) (sort '< '(1 2 3)))
   (vlu-assert-equal '(3 2 1) (sort '> '(1 2 3)))

   (vlu-assert-equal '(1 1 2 2 3 3) (sort '< '(1 1 2 2 3 3)))
   (vlu-assert-equal '(3 3 2 2 1 1) (sort '> '(1 1 2 2 3 3)))

   (vlu-assert-equal '(1 2 3 4 5) (sort '< '(2 3 1 5 4)))

   (vlu-assert-equal '("1" "2" "3") (sort '< '("2" "3" "1")))
   (vlu-assert-equal '("3" "2" "1") (sort '> '("2" "3" "1")))
))

(vlu-add-test
 (defun usort-test ()
   (vlu-assert-equal '() (usort '< '()))

   (vlu-assert-equal '(1 2 3) (usort '< '(1 2 3)))
   (vlu-assert-equal '(3 2 1) (usort '> '(1 2 3)))

   (vlu-assert-equal '(1 2 3) (usort '< '(1 1 2 2 3 3)))
   (vlu-assert-equal '(3 2 1) (usort '> '(1 1 2 2 3 3)))

   (vlu-assert-equal '(1 2 3 4 5) (usort '< '(2 3 1 5 4)))

   (vlu-assert-equal '("1" "2" "3") (usort '< '("2" "3" "1")))
   (vlu-assert-equal '("3" "2" "1") (usort '> '("2" "3" "1")))
))

(vlu-add-test
 (defun uniq-test ()
   (vlu-assert-equal '() (uniq '()))

   (vlu-assert-equal '(1 2 3) (uniq '(1 2 3)))
   (vlu-assert-equal '(3 2 1) (uniq '(3 2 1)))

   (vlu-assert-equal '(1 2 3) (uniq '(1 1 2 2 3 3)))
   (vlu-assert-equal '(3 2 1) (uniq '(3 3 2 2 1 1)))
))
