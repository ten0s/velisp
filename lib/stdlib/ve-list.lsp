;; VeLisp Extension
(defun join (delim lst / acc)
  (if (= (length lst) 1) (setq acc lst)
    (progn
      (setq acc '())
      (foreach item lst
               ;; Add item and then delim
               (setq acc (cons delim (cons item acc))))
      ;; Remove last delim and reverse
      (setq acc (reverse (cdr acc)))))
  (apply 'strcat
         (mapcar 'vl-princ-to-string acc)))

;; VeLisp Extension
(defun sort (cmp lst / insert len)
  ;; Sorts the elements in a list by the insertion sort
  ;; according to a given compare function
  (defun insert (item sorted-lst)
    (cond ((null sorted-lst) (list item))
          ((cmp item (car sorted-lst)) (cons item sorted-lst))
          (t (cons (car sorted-lst)
                   (insert item (cdr sorted-lst))))))
  (setq len (length lst))
  (cond ((= len 0) lst)
        ((= len 1) lst)
        (T (insert (car lst) (sort cmp (cdr lst))))))

;; VeLisp Extension
(defun usort (cmp lst / uinsert len)
  ;; Uniquely sorts the elements in a list by the insertion sort
  ;; according to a given compare function
  (defun uinsert (item sorted-lst)
    (cond ((null sorted-lst) (list item))
          ((equal item (car sorted-lst)) sorted-lst)
          ((cmp item (car sorted-lst)) (cons item sorted-lst))
          (t (cons (car sorted-lst)
                   (uinsert item (cdr sorted-lst))))))
  (setq len (length lst))
  (cond ((= len 0) lst)
        ((= len 1) lst)
        (T (uinsert (car lst) (usort cmp (cdr lst))))))

;; VeLisp Extension
(defun uniq (lst / set)
  ;; Removes duplicates from a list
  (setq set '())
  (foreach item lst
           (if (not (member item set))
               (setq set (cons item set))))
  (reverse set))

;; VeLisp Extension
(defun enumerate (lst / i acc)
  ;; Adds a zero-based counter to each item in a list
  (setq i 0)
  (foreach item lst
           (setq acc (cons (cons i item) acc)
                 i (1+ i)))
  (reverse acc))

;; VeLisp Extension
(defun shuffle (lst)
  ;; Shuffles randomly the elements in a list
  (mapcar 'cdr
          (sort (lambda (l r) (< (car l) (car r)))
                (mapcar '(lambda (x) (cons (rand) x)) lst))))
