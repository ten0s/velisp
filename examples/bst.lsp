;;;; SPDX-License-Identifier: 0BSD

;;;;
;;;; Binary Search Tree Data Structure
;;;; BST = nil | (key val BST BST)
;;;;

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

(if (not take)
    (defun take (n lst / acc)
      ;; Returns the first n elements in a list
      (while (and (not (null lst)) (> n 0))
        (setq acc (cons (car lst) acc)
              lst (cdr lst)
              n (1- n)))
      (reverse acc)))

(if (not drop)
    (defun drop (n lst)
      ;; Returns a sublist with the first n elements dropped
      (while (and (not (null lst)) (> n 0))
        (setq lst (cdr lst)
              n (1- n)))
      lst))

(if (not sublist)
    (defun sublist (start len lst)
      ;; Returns a sublist of a list
      (take len (drop start lst))))

;;;;
;;;; API
;;;;

;;; () -> BST
(defun bst_new ()
  nil)

;;; (BST) -> T | nil
(defun bst_is_empty (bst)
  (null bst))

;;; Build BST from sorted by key list:
;;; (((k . v) | (k v ...))) -> BST
(defun bst_from_list (lst / len mid c l r)
  (if (null lst)
      (bst_new)
    (progn
      (setq len (length lst)
            mid (/ len 2)
            c   (nth mid lst)
            l   (sublist 0 mid lst)
            r   (sublist (1+ mid) len lst))
      (bst_make_node (car c)
                     (cdr c)
                     (bst_from_list l)
                     (bst_from_list r)))))

;;; (BST) -> ((k . v) | (k v ...))
(defun bst_to_list (bst / k v l r)
  (if (bst_is_empty bst)
      nil
    (progn
      (setq k (bst_node_key bst)
            v (bst_node_value bst)
            l (bst_node_left bst)
            r (bst_node_right bst))
      (append (bst_to_list l)
              (list (cons k v))
              (bst_to_list r)))))

;;; (key BST) -> val | nil
(defun bst_get (key bst / k)
  (if (bst_is_empty bst)
      nil
    (progn
      (setq k (bst_node_key bst))
      (cond ((< key k) (bst_get key (bst_node_left bst)))
            ((> key k) (bst_get key (bst_node_right bst)))
            (T         (bst_node_value bst))))))

;;; (key val BST) -> BST'
(defun bst_set (key val bst / k v l r)
  (if (bst_is_empty bst)
      (bst_make_node key val nil nil)
    (progn
      (setq k (bst_node_key bst)
            v (bst_node_value bst)
            l (bst_node_left bst)
            r (bst_node_right bst))
      (cond ((< key k) (bst_make_node k v (bst_set key val l) r))
            ((> key k) (bst_make_node k v l (bst_set key val r)))
            (T         (bst_make_node key val l r))))))

;;; (fun BST) -> BST'
;;; fun :: (key val) -> val'
(defun bst_map (fun bst / k v l r)
  (if (bst_is_empty bst)
      bst
    (progn
      (setq k (bst_node_key bst)
            v (bst_node_value bst)
            l (bst_node_left bst)
            r (bst_node_right bst))
      (bst_make_node k
                     (fun k v)
                     (bst_map fun l)
                     (bst_map fun r)))))

;;; TODO bst_fold

;;;;
;;;; Internal
;;;;

(defun bst_make_node (key val left right)
  (list key val left right))

(defun bst_node_key (bst)
  (car bst))

(defun bst_node_value (bst)
  (cadr bst))

(defun bst_node_left (bst)
  (caddr bst))

(defun bst_node_right (bst)
  (cadddr bst))

;;;;
;;;; Tests
;;;;

;(setq bst (bst_new))
;(setq bst (bst_set 1 'one bst))
;(setq bst (bst_set 0 'zero bst))
;(setq bst (bst_set 2 'two bst))
