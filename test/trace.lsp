;; https://www.csie.ntu.edu.tw/~course/10420/Resources/lp/node32.html
(defun power (x y)
  (if (= y 0) 1
    (* x (power x (- y 1)))))

;; https://www2.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html
(defun fibonacci (N)
  "Compute the N'th Fibonacci number."
  (if (or (zerop N) (= N 1))
      1
    (+ (fibonacci (- N 1)) (fibonacci (- N 2)))))

;; With trace
(princ "Test #1\n")
(trace power)
(princ (power 3 4))
(princ "\n")
(untrace power)

;; Without trace
(princ "Test #2\n")
(princ (power 3 4))
(princ "\n")

;; With trace
(princ "Test #3\n")
(trace fibonacci)
(princ (fibonacci 3))
(princ "\n")
(untrace fibonacci)
