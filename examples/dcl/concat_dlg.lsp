; (action_tile "edit1" "(set_tile \"text1\" (get_tile \"edit1\"))")
(action_tile "edit1" "(set_tile \"text1\" $value)")
(action_tile "edit2" "(set_tile \"text2\" $value)")

(defun concat ()
  (strcat (get_tile "edit1") " " (get_tile "edit2")))

(action_tile "accept" "(set_tile \"text1\" (concat)) (set_tile \"text2\" (concat))")
(action_tile "cancel" "(done_dialog 0)")
