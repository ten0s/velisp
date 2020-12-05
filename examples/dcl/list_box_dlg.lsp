(set_tile "listbox1" "") ; TODO: doesn't work for single
(set_tile "listbox1-value" (get_tile "listbox1"))

(set_tile "listbox2-value" (get_tile "listbox2"))
(action_tile "listbox2" "(set_tile \"listbox2-value\" $value)")

(set_tile "listbox3" "0 6 11")
(set_tile "listbox3-value" (get_tile "listbox3"))
(action_tile "listbox3" "(set_tile \"listbox3-value\" $value)")
