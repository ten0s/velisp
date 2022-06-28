;;;; SPDX-License-Identifier: 0BSD

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

(vlu-add-test
 (defun enumerate-test ()
   (vlu-assert-equal '() (enumerate '()))
   (vlu-assert-equal '((0 . 1) (1 . 2) (2 . 3)) (enumerate '(1 2 3)))
   (vlu-assert-equal '((0 . 3) (1 . 2) (2 . 1)) (enumerate '(3 2 1)))
   (vlu-assert-equal '((0 . "a") (1 . "b") (2 . "c")) (enumerate '("a" "b" "c")))
   (vlu-assert-equal '((0 . a) (1 . b) (2 . c)) (enumerate '(a b c)))
))

(vlu-add-test
 (defun shuffle-test ()
   (vlu-assert-equal '() (shuffle '()))
   (vlu-assert-equal '(1) (shuffle '(1)))
   (srand 0)
   (vlu-assert-equal '(1 4 2 5 3) (shuffle '(1 2 3 4 5)))
   (vlu-assert-equal '(2 4 1 5 3) (shuffle '(1 2 3 4 5)))))

(vlu-add-test
 (defun take-test ()
   (vlu-assert-equal '() (take -1 '()))
   (vlu-assert-equal '() (take 0 '()))
   (vlu-assert-equal '() (take 1 '()))

   (vlu-assert-equal '() (take -1 '(1 2 3)))
   (vlu-assert-equal '() (take 0 '(1 2 3)))
   (vlu-assert-equal '(1) (take 1 '(1 2 3)))

   (vlu-assert-equal '(1 2 3) (take 3 '(1 2 3)))
   (vlu-assert-equal '(1 2 3) (take 5 '(1 2 3)))))

(vlu-add-test
 (defun drop-test ()
   (vlu-assert-equal '() (drop -1 '()))
   (vlu-assert-equal '() (drop 0 '()))
   (vlu-assert-equal '() (drop 1 '()))

   (vlu-assert-equal '(1 2 3) (drop -1 '(1 2 3)))
   (vlu-assert-equal '(1 2 3) (drop 0 '(1 2 3)))
   (vlu-assert-equal '(2 3) (drop 1 '(1 2 3)))

   (vlu-assert-equal '(3) (drop 2 '(1 2 3)))
   (vlu-assert-equal '() (drop 5 '(1 2 3)))))

(vlu-add-test
 (defun sublist-test ()
   (vlu-assert-equal '() (sublist -1 0 '()))
   (vlu-assert-equal '() (sublist 0 -1 '()))
   (vlu-assert-equal '() (sublist 0 0 '()))
   (vlu-assert-equal '() (sublist 1 0 '()))
   (vlu-assert-equal '() (sublist 0 1 '()))

   (vlu-assert-equal '() (sublist 0 0 '(1 2 3 4 5)))
   (vlu-assert-equal '() (sublist 1 0 '(1 2 3 4 5)))

   (vlu-assert-equal '(1) (sublist 0 1 '(1 2 3 4 5)))
   (vlu-assert-equal '(1 2 3 4 5) (sublist 0 5 '(1 2 3 4 5)))
   (vlu-assert-equal '(1 2 3 4 5) (sublist 0 10 '(1 2 3 4 5)))
   (vlu-assert-equal '(2 3 4 5) (sublist 1 4 '(1 2 3 4 5)))

   (vlu-assert-equal '(5) (sublist 4 1 '(1 2 3 4 5)))
   (vlu-assert-equal '() (sublist 5 10 '(1 2 3 4 5)))))
