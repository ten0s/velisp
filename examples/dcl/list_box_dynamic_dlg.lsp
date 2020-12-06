; Clear/append
(start_list "listbox1" 3)
  (add_list "ONE")
  (add_list "TWO")
  (add_list "THREE")
(end_list)
(set_tile "listbox1" "0")
(set_tile "listbox1-value" (get_tile "listbox1"))
(action_tile "listbox1" "(set_tile \"listbox1-value\" $value)")

; Change
(start_list "listbox2" 1 1)
  (add_list "TWO")
(end_list)
(set_tile "listbox2" "1")
(set_tile "listbox2-value" (get_tile "listbox2"))
(action_tile "listbox2" "(set_tile \"listbox2-value\" $value)")

; Append
(start_list "listbox3" 2)
  (add_list "THREE")
(end_list)
(set_tile "listbox3" "2")
(set_tile "listbox3-value" (get_tile "listbox3"))
(action_tile "listbox3" "(set_tile \"listbox3-value\" $value)")
