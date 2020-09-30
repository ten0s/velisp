(defun atom (item)
  (not (equal (type item) 'list)))

(defun not (item)
  (equal item nil))

(defun null (item)
  (not item))
