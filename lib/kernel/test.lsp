(vlu-add-test
 (defun atom-test ( / a b)
   (setq a '(x y z))
   (setq b 'a)
   (vlu-assert     (atom 'a))
   (vlu-assert-not (atom a))
   (vlu-assert     (atom 'b))
   (vlu-assert     (atom b))
   (vlu-assert-not (atom '(a b c)))))

(vlu-add-test
 (defun not-test ()
   (vlu-assert     (not nil))
   (vlu-assert-not (not t))
   (vlu-assert-not (not 0))
   (vlu-assert-not (not 0.0))
   (vlu-assert-not (not ""))
   (vlu-assert-not (not 'foo))
   (vlu-assert     (not '()))
   (vlu-assert-not (not '(1 2 3)))
   (vlu-assert-not (not '(1 . 2)))))

(vlu-add-test
 (defun null-test ()
   (vlu-assert     (null nil))
   (vlu-assert-not (null t))
   (vlu-assert-not (null 0))
   (vlu-assert-not (null 0.0))
   (vlu-assert-not (null ""))
   (vlu-assert-not (null 'foo))
   (vlu-assert     (null '()))
   (vlu-assert-not (null '(1 2 3)))
   (vlu-assert-not (null '(1 . 2)))))

(vlu-add-test
 (defun listp-test ()
   (vlu-assert     (listp nil))
   (vlu-assert     (listp '(a b c)))
   (vlu-assert     (listp '(a . b)))
   (vlu-assert-not (listp 'a))
   (vlu-assert-not (listp 4.343))))

(vlu-add-test
 (defun minusp-test ()
   (vlu-assert     (minusp -1))
   (vlu-assert     (minusp -1.0))
   (vlu-assert-not (minusp 0))
   (vlu-assert-not (minusp 0.0))
   (vlu-assert-not (minusp 1))
   (vlu-assert-not (minusp 1.0))))

(vlu-add-test
 (defun zerop-test ()
   (vlu-assert-not (zerop -1))
   (vlu-assert-not (zerop -1.0))
   (vlu-assert     (zerop 0))
   (vlu-assert     (zerop 0.0))
   (vlu-assert-not (zerop 1))
   (vlu-assert-not (zerop 1.0))))

(vlu-add-test
 (defun numberp-test ()
   (vlu-assert     (numberp 1))
   (vlu-assert     (numberp 1.0))
   (vlu-assert-not (numberp "0"))
   (vlu-assert-not (numberp t))
   (vlu-assert-not (numberp nil))
   (vlu-assert-not (numberp '(1 2 3)))))

(vlu-add-test
 (defun inc-test ()
   (vlu-assert-equal   0 (1+ -1))
   (vlu-assert-equal   1 (1+ 0))
   (vlu-assert-equal   2 (1+ 1))
   (vlu-assert-equal 0.0 (1+ -1.0))
   (vlu-assert-equal 1.0 (1+ 0.0))
   (vlu-assert-equal 2.0 (1+ 1.0))))

(vlu-add-test
 (defun dec-test ()
   (vlu-assert-equal   -1  (1- 0))
   (vlu-assert-equal    0 (1- 1))
   (vlu-assert-equal    1 (1- 2))
   (vlu-assert-equal -1.0 (1- 0.0))
   (vlu-assert-equal  0.0 (1- 1.0))
   (vlu-assert-equal  1.0 (1- 2.0))))

(vlu-add-test
 (defun abs-test ()
   (vlu-assert-equal   1 (abs -1))
   (vlu-assert-equal   0 (abs 0))
   (vlu-assert-equal   1 (abs 1))
   (vlu-assert-equal 1.0 (abs -1.0))
   (vlu-assert-equal 0.0 (abs 0.0))
   (vlu-assert-equal 1.0 (abs 1.0))))

(vlu-add-test
 (defun float-test ()
   (vlu-assert-equal -1.0 (float -1))
   (vlu-assert-equal  0.0 (float 0))
   (vlu-assert-equal  1.0 (float 1))
   (vlu-assert-equal -1.0 (float -1.0))
   (vlu-assert-equal  0.0 (float 0.0))
   (vlu-assert-equal  1.0 (float 1.0))))

(vlu-add-test
 (defun gcd-test ()
   (vlu-assert-equal 3 (gcd 81 57))
   (vlu-assert-equal 4 (gcd 12 20))))
