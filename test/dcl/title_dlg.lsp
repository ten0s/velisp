(setq index 1)
(setq prefix "Title")

(defun title (prefix index)
  (strcat prefix " " (itoa index)))

(defun change ( / old_title new_title)
  (setq old_title (get_tile "dialog"))
  (setq index (1+ index))
  (setq new_title (title prefix index))
  (set_tile "old_title" old_title)
  (set_tile "dialog" new_title))

(set_tile "dialog" (title prefix index))
(set_tile "old_title" (title prefix (1- index)))
(action_tile "change" "(change)")
(action_tile "cancel" "(done_dialog 0)")
