;;;; SPDX-License-Identifier: 0BSD

(defun fib (n / iter)
  (defun iter (a b counter)
    (if (= counter 0)
      a
      (iter b (+ a b) (- counter 1))))
  (iter 0 1 n))

(setq n 10)
(if (> (length (argv)) 2)
    (setq n (atoi (argv 2))))

(princ (fib n))
(princ "\n")
