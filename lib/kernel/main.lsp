(defun atom (item)
  (not (equal (type item) 'list)))

(defun not (item)
  (equal item nil))

(defun null (item)
  (not item))

(defun listp (item)
  (or (not item) (not (atom item))))
