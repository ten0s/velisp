;;;; SPDX-License-Identifier: 0BSD

(vlu-add-test
 (defun rand-test ()
   (srand 0)
   (vlu-assert-equal 38 (rand))
   (vlu-assert-equal 7719 (rand))
   (vlu-assert-equal 21238 (rand))
   (vlu-assert-equal 2437 (rand))
   (vlu-assert-equal 8855 (rand))
   (srand 1)
   (vlu-assert-equal 41 (rand))
   (vlu-assert-equal 18467 (rand))
   (vlu-assert-equal 6334 (rand))
   (vlu-assert-equal 26500 (rand))
   (vlu-assert-equal 19169 (rand))
   (srand 1.9) ; will fix to 1
   (vlu-assert-equal 41 (rand))
   (vlu-assert-equal 18467 (rand))
   (vlu-assert-equal 6334 (rand))
   (vlu-assert-equal 26500 (rand))
   (vlu-assert-equal 19169 (rand))))
