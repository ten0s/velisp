; Clear/append
(start_list "popup1" 3)
  (add_list "ONE")
  (add_list "TWO")
  (add_list "THREE")
(end_list)
(set_tile "popup1" "0")
(set_tile "popup1-value" (get_tile "popup1"))
(action_tile "popup1" "(set_tile \"popup1-value\" $value)")

; Change
(start_list "popup2" 1 1)
  (add_list "TWO")
(end_list)
(set_tile "popup2" "1")
(set_tile "popup2-value" (get_tile "popup2"))
(action_tile "popup2" "(set_tile \"popup2-value\" $value)")

; Append
(start_list "popup3" 2)
  (add_list "THREE")
(end_list)
(set_tile "popup3" "2")
(set_tile "popup3-value" (get_tile "popup3"))
(action_tile "popup3" "(set_tile \"popup3-value\" $value)")
