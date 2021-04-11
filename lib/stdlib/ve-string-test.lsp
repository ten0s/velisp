(vlu-add-test
 (defun ve-string-split-test ()
   (vlu-assert-equal '() (ve-string-split "" ""))
   (vlu-assert-equal '("a") (ve-string-split "" "a"))
   (vlu-assert-equal '("a" "b" "c") (ve-string-split "" "abc"))

   (vlu-assert-equal '("") (ve-string-split " " ""))
   (vlu-assert-equal '("a") (ve-string-split " " "a"))
   (vlu-assert-equal '("a" "b" "c") (ve-string-split " " "a b c"))

   (vlu-assert-equal '("") (ve-string-split "," ""))
   (vlu-assert-equal '("" "") (ve-string-split "," ","))
   (vlu-assert-equal '("1" "2" "3") (ve-string-split "," "1,2,3"))
   (vlu-assert-equal '("1" "2" "3" "" "") (ve-string-split "," "1,2,3,,"))
   (vlu-assert-equal '("1" "2" "" "3" "" "") (ve-string-split "," "1,2,,3,,"))

   (vlu-assert-equal
    '("Hello" "World!" "It's me" "" "again:)")
    (ve-string-split "\n" "Hello\nWorld!\nIt's me\n\nagain:)"))
   ))
