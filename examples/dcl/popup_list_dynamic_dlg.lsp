; Clear/append
(start_list "popup1" START_LIST_CLEAR)
  (mapcar 'add_list '("ONE" "TWO" "THREE"))
(end_list)
(set_tile "popup1" "0")
(set_tile "popup1-value" (get_tile "popup1"))
(action_tile "popup1" "(set_tile \"popup1-value\" $value)")

; Change
(start_list "popup2" START_LIST_CHANGE 1)
  (add_list "TWO")
(end_list)
(set_tile "popup2" "1")
(set_tile "popup2-value" (get_tile "popup2"))
(action_tile "popup2" "(set_tile \"popup2-value\" $value)")

; Append
(start_list "popup3" START_LIST_APPEND)
  (add_list "THREE")
(end_list)
(set_tile "popup3" "2")
(set_tile "popup3-value" (get_tile "popup3"))
(action_tile "popup3" "(set_tile \"popup3-value\" $value)")
