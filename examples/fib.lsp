(defun fib (n / iter)
  (defun iter (a b counter)
    (if (= counter 0)
      a
      (iter b (+ a b) (- counter 1))))
  (iter 0 1 n))

(princ (fib 10))
