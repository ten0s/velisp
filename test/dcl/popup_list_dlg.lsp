(set_tile "popup1-value" (get_tile "popup1"))

(action_tile "popup2" "(set_tile \"popup2-value\" $value)");
(set_tile "popup2" "0")

(set_tile "popup3" "")
(action_tile "popup3" "(set_tile \"popup3-value\" $value)");
