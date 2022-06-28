;;;; SPDX-License-Identifier: 0BSD

(defun vl-member-if (predicate lst)
  (if (null lst) nil
    (if (predicate (car lst)) lst
      (vl-member-if predicate (cdr lst)))))

(defun vl-member-if-not (predicate-if-not lst)
  ;; Since there's not function closure implemented
  ;; 'predicate-if-not' must not be equal to 'predicate',
  ;; otherwise the stack overflow occurs
  (vl-member-if (lambda (x) (not (predicate-if-not x))) lst))

(defun vl-position (elm lst / index)
  (defun index (pos lst)
    (cond ((null lst) nil)
          ((equal elm (car lst)) pos)
          (t (index (1+ pos) (cdr lst)))))
  (index 0 lst))

(defun vl-remove (elm lst)
  (vl-remove-if (lambda (x) (equal x elm)) lst))

(defun vl-remove-if (predicate lst)
  (if (null lst) nil
    (if (predicate (car lst)) (vl-remove-if predicate (cdr lst))
      (cons (car lst)
            (vl-remove-if predicate (cdr lst))))))

(defun vl-remove-if-not (predicate-if-not lst)
  ;; Since there's not function closure implemented
  ;; 'predicate-if-not' must not be equal to 'predicate',
  ;; otherwise the stack overflow occurs
  (vl-remove-if (lambda (x) (not (predicate-if-not x))) lst))

(defun vl-sort (lst cmp)
  (sort cmp lst))

(defun vl-sort-i (lst cmp-i)
  ;; Since there's not function closure implemented
  ;; 'cmp-i' must not be equal to 'cmp'
  (mapcar 'car
          (sort (lambda (x y) (cmp-i (cdr x) (cdr y)))
                (enumerate lst))))
