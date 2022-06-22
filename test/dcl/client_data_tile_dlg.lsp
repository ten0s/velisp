(defun toggle ( / btn_label)
   (setq label (get_tile "toggle"))
   (cond ((= label "Set data")
          (client_data_tile "button" "userdata")
          (set_tile "toggle" "Unset data"))
         ((= label "Unset data")
          (client_data_tile "button" "")
          (set_tile "toggle" "Set data"))))

(set_tile "toggle" "Set data")
(action_tile "toggle" "(toggle)")

(action_tile "button" "(set_tile \"data\" $data)")
