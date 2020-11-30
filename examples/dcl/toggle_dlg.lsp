(defun on_off (val)
   (if (= val "1") "on" "off"))

(set_tile "toggle1-value" (on_off (get_tile "toggle1")))
(set_tile "toggle2-value" (on_off (get_tile "toggle2")))
(action_tile "toggle2" "(set_tile \"toggle2-value\" (on_off $value))")
