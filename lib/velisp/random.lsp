;;;; SPDX-License-Identifier: 0BSD

;;;;
;;;; Pseudo-random number generator
;;;; https://en.wikipedia.org/wiki/Linear_congruential_generator
;;;; https://rosettacode.org/wiki/Linear_congruential_generator
;;;; Microsoft formula
;;;;

;;; (Int | Real) -> Int
(defun srand (seed / m)
  ;; Initializes pseudo-random number generator
  (setq m 2147483648
        %VELISP_RAND_SEED% (rem (fix seed) m)))
(srand 0)

;;; () -> 0 ... 32767
(defun rand ( / a c m d)
  ;; Returns a pseudo-random integral number in the range between 0 and 32767
  (setq a 214013
        c 2531011
        m 2147483648
        d 65536
        %VELISP_RAND_SEED% (rem (+ (* %VELISP_RAND_SEED% a) c) m))
  (/ %VELISP_RAND_SEED% d))
