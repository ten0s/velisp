(defun fib (n)
  (defun fib-iter (a b counter)
    (if (= counter 0)
      a
      (fib-iter b (+ a b) (- counter 1))))
  (fib-iter 0 1 n))

(princ (fib 10))
