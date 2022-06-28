;;;; SPDX-License-Identifier: 0BSD

(vlu-add-test
 (defun split-test ()
   (vlu-assert-equal '() (split "" ""))
   (vlu-assert-equal '("a") (split "" "a"))
   (vlu-assert-equal '("a" "b" "c") (split "" "abc"))

   (vlu-assert-equal '("") (split " " ""))
   (vlu-assert-equal '("a") (split " " "a"))
   (vlu-assert-equal '("a" "b" "c") (split " " "a b c"))

   (vlu-assert-equal '("") (split "," ""))
   (vlu-assert-equal '("" "") (split "," ","))
   (vlu-assert-equal '("1" "2" "3") (split "," "1,2,3"))
   (vlu-assert-equal '("1" "2" "3" "" "") (split "," "1,2,3,,"))
   (vlu-assert-equal '("1" "2" "" "3" "" "") (split "," "1,2,,3,,"))

   (vlu-assert-equal
    '("Hello" "World!" "It's me" "" "again:)")
    (split "\n" "Hello\nWorld!\nIt's me\n\nagain:)"))
   ))
