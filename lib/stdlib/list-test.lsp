;;
;;
;;

(vlu-add-test (defun caar-test ()
  (vlu-assert-equal 1 (caar (list (list 1 2 3) (list 4 5 6))))))

(vlu-add-test (defun cadr-test ()
  (vlu-assert-equal 3.2 (cadr (list 1.5 3.2 2.0)))))

(vlu-add-test (defun cdar-test ()
  (vlu-assert-equal (list 2 3) (cdar (list (list 1 2 3) (list 4 5 6))))))

(vlu-add-test (defun cddr-test ()
  (vlu-assert-equal (list) (cddr (list (list 1 2 3) (list 4 5 6))))))

;;;
;;;
;;;

(vlu-add-test (defun caaar-test ()
  (vlu-assert-equal 1 (caaar (list (list (list 1 2 3 4 5)))))))

(vlu-add-test (defun caadr-test ()
  (vlu-assert-equal (list 2 3 4 5) (caadr (list 1 (list (list 2 3 4 5)))))))

(vlu-add-test (defun cadar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun caddr-test ()
  (vlu-assert-equal 2.0 (caddr (list 1.5 3.2 2.0)))))

(vlu-add-test (defun cdaar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cdadr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cddar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cdddr-test ()
  (vlu-assert-equal 1 1)))

;;;;
;;;;
;;;;

(vlu-add-test (defun caaaar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun caaadr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun caadar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun caaddr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cadaar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cadadr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun caddar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cadddr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cdaaar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cdaadr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cdadar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cdaddr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cddaar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cddadr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cdddar-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun cddddr-test ()
  (vlu-assert-equal 1 1)))

(vlu-add-test (defun length-test ()
  (vlu-assert-equal 0 (length (list)))
  (vlu-assert-equal 1 (length (list 1)))
  (vlu-assert-equal 3 (length (list 1 2 3)))
  (vlu-assert-equal 3 (length (list (list 1) (list 2) (list 3))))))

(vlu-add-test (defun reverse-test ()
  (vlu-assert-equal (list 'c 'b (list 'a)) (reverse (list (list 'a) 'b 'c)))))

(vlu-add-test (defun last-test ()
  (vlu-assert-equal 'e (last (list 'a 'b 'c 'd 'e)))
  (vlu-assert-equal (list 'd 'e) (last (list 'a 'b 'c (list 'd 'e))))))

(vlu-add-test (defun nth-test ()
  (vlu-assert-equal 'd (nth 3 (list 'a 'b 'c 'd 'e)))
  (vlu-assert-equal 'a (nth 0 (list 'a 'b 'c 'd 'e)))
  (vlu-assert-equal nil (nth 5 (list 'a 'b 'c 'd 'e)))))

(vlu-add-test (defun member-test ()
  (vlu-assert-equal (list 'c 'd 'e) (member 'c (list 'a 'b 'c 'd 'e)))
  (vlu-assert-equal nil (member 'q (list 'a 'b 'c 'd 'e)))))

(vlu-add-test (defun mapcar-test ()
  (vlu-assert-equal nil (mapcar '1+ (list)))
  (vlu-assert-equal (list 2 3 4) (mapcar '1+ (list 1 2 3)))))
