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
