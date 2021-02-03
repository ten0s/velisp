(set_tile "slider1" "5000")

(set_tile "slider1-value" (get_tile "slider1"))
(set_tile "slider2-value" (get_tile "slider2"))

(action_tile "slider2" "(set_tile \"slider2-value\" $value)")
