;;;; SPDX-License-Identifier: 0BSD

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

(defun sort (cmp lst / insert len)
  "Sorts the elements in a list by the insertion sort"
  "according to a given compare function             "
  (defun insert (item sorted-lst)
    (cond ((null sorted-lst) (list item))
          ((cmp item (car sorted-lst)) (cons item sorted-lst))
          (T (cons (car sorted-lst)
                   (insert item (cdr sorted-lst))))))
  (setq len (length lst))
  (cond ((= len 0) lst)
        ((= len 1) lst)
        (T (insert (car lst) (sort cmp (cdr lst))))))

(defun usort (cmp lst / uinsert len)
  "Uniquely sorts the elements in a list by the insertion sort"
  "according to a given compare function                      "
  (defun uinsert (item sorted-lst)
    (cond ((null sorted-lst) (list item))
          ((equal item (car sorted-lst)) sorted-lst)
          ((cmp item (car sorted-lst)) (cons item sorted-lst))
          (T (cons (car sorted-lst)
                   (uinsert item (cdr sorted-lst))))))
  (setq len (length lst))
  (cond ((= len 0) lst)
        ((= len 1) lst)
        (T (uinsert (car lst) (usort cmp (cdr lst))))))

(defun uniq (lst / set)
  "Removes duplicates from a list"
  (setq set '())
  (foreach item lst
           (if (not (member item set))
               (setq set (cons item set))))
  (reverse set))

(defun enumerate (lst / i acc)
  "Adds a zero-based counter to each item in a list"
  (setq i 0)
  (foreach item lst
           (setq acc (cons (cons i item) acc)
                 i (1+ i)))
  (reverse acc))

(defun shuffle (lst)
  "Shuffles randomly the elements in a list"
  (mapcar 'cdr
          (sort (lambda (l r) (< (car l) (car r)))
                (mapcar '(lambda (x) (cons (rand) x)) lst))))

(defun take (n lst / acc)
  "Returns the first n elements in a list"
  ;; TODO: assert n is int
  (while (and (not (null lst)) (> n 0))
    (setq acc (cons (car lst) acc)
          lst (cdr lst)
          n (1- n)))
  (reverse acc))

(defun drop (n lst)
  "Returns a sublist with the first n elements dropped"
  ;; TODO: assert n is int
  (while (and (not (null lst)) (> n 0))
    (setq lst (cdr lst)
          n (1- n)))
  lst)

(defun sublist (start len lst)
  "Returns a sublist of a list"
  (take len (drop start lst)))

(defun seq (from to step / lst cmp)
  "Returns a list of integers that starts with 'from' and "
  "contains the successive results of adding 'step' to the"
  "previous element, until 'to' is reached or passed      "
  (setq lst '())
  (cond ((and (< from to) (> step 0)) (setq cmp <=))
        ((and (> from to) (< step 0)) (setq cmp >=))
        ((and (= from to)) (setq cmp <= step 1))
        ;; TODO: better throw error
        (t (setq cmp (lambda (x y) nil))))
  (while (cmp from to)
    (setq lst (cons from lst)
          from (+ from step)))
  (reverse lst))
