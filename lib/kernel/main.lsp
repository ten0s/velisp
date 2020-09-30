(defun atom (item)
  (not (equal (type item) 'list)))

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
