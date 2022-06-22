(defun getint ( / str1 str2 int)
  (setq str1 (get_tile "edit"))
  (setq int (atoi str1))
  (setq str2 (itoa int))
  (if (and (>= int 0) (= str1 str2))
    (progn
      (set_tile "error" "")
      (set_tile "text" (itoa int)))
    (progn
      (set_tile "error" "Invalid Integer")
      (set_tile "text" "")
      (mode_tile "edit" 2))))

(action_tile "getint" "(getint)")
(action_tile "cancel" "(done_dialog 0)")
