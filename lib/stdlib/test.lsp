(defun length-test ()
  (vlu-assert-equal 0 (length (list)))
  (vlu-assert-equal 1 (length (list 1)))
  (vlu-assert-equal 3 (length (list 1 2 3)))
  (vlu-assert-equal 3 (length (list (list 1) (list 2) (list 3)))))

(defun reverse-test ()
  (vlu-assert-equal (list 'c 'b (list 'a)) (reverse (list (list 'a) 'b 'c))))

(defun last-test ()
  (vlu-assert-equal 'e (last (list 'a 'b 'c 'd 'e)))
  (vlu-assert-equal (list 'd 'e) (last (list 'a 'b 'c 'd 'e))))

(defun nth-test ()
  (vlu-assert-equal 'd (nth 3 (list 'a 'b 'c 'd 'e)))
  (vlu-assert-equal 'a (nth 0 (list 'a 'b 'c 'd 'e)))
  (vlu-assert-equal nil (nth 5 (list 'a 'b 'c 'd 'e))))

(defun member-test ()
  (vlu-assert-equal (list 'c 'd 'e) (member 'c (list 'a 'b 'c 'd 'e)))
  (vlu-assert-equal nil (member 'q (list 'a 'b 'c 'd 'e))))

(defun mapcar-test ()
  (vlu-assert-equal nil (mapcar '1+ (list)))
  (vlu-assert-equal (list 2 3 4) (mapcar '1+ (list 1 2 3))))

(vlu-remove-tests)

(vlu-add-test 'length-test)
(vlu-add-test 'reverse-test)
(vlu-add-test 'nth-test)
(vlu-add-test 'member-test)
(vlu-add-test 'mapcar-test)

(vlu-run-tests)
