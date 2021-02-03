(set_tile "radio2" "1")
(mode_tile "radio4" 1)
(set_tile "current" (get_tile "radio_group"))
(action_tile "radio_group" "(set_tile \"current\" $value)")
