(vlu-add-test
 (defun atom-test ( / a b)
   (setq a '(x y z))
   (setq b 'a)
   (vlu-assert-equal t (atom 'a))
   (vlu-assert-equal nil (atom a))
   (vlu-assert-equal t (atom 'b))
   (vlu-assert-equal t (atom b))
   (vlu-assert-equal nil (atom '(a b c)))))
