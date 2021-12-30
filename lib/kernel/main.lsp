(defun atom (item)
  (or (null item) (not (equal (type item) 'list))))

(defun not (item)
  (equal item nil))

(defun null (item)
  (not item))

(defun listp (item)
  (or (not item) (not (atom item))))

(defun minusp (num)
  (< num 0))

(defun zerop (num)
  (= num 0))

(defun numberp (item / typ)
  (setq typ (type item))
  (or (equal typ 'int) (equal typ 'real)))

(defun 1+ (num)
  (+ num 1))

(defun 1- (num)
  (- num 1))

(defun abs (num)
  (if (< num 0) (* -1 num)
    num))

(defun float (num)
  (* num 1.0))

(defun gcd (a b)
  (if (= b 0) (abs a)
    (gcd b (rem a b))))

;;;
;;; Pseudo-random number generator
;;; https://en.wikipedia.org/wiki/Linear_congruential_generator
;;; https://rosettacode.org/wiki/Linear_congruential_generator
;;; Microsoft formula
;;;

;; (Int | Real) -> Int
(defun srand (seed / m)
  ;; Initializes pseudo-random number generator
  (setq m 2147483648
        %VELISP_RAND_SEED% (rem (fix seed) m)))
(srand 0)

;; () -> 0 ... 32767
(defun rand ( / a c m d)
  ;; Returns a pseudo-random integral number in the range between 0 and 32767
  (setq a 214013
        c 2531011
        m 2147483648
        d 65536
        %VELISP_RAND_SEED% (rem (+ (* %VELISP_RAND_SEED% a) c) m))
  (/ %VELISP_RAND_SEED% d))
