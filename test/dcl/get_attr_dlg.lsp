(setq keys '("get_attr_dialog" "key_name" "attr_name" "attr_value" "get_attr"))
(setq attrs '("key" "label" "value" "width" "height" "unknown"))

(start_list "key_name")
  (mapcar 'add_list keys)
(end_list)
(set_tile "key_name" "0")

(start_list "attr_name")
  (mapcar 'add_list attrs)
(end_list)
(set_tile "attr_name" "0")

(defun get_attr_value ( / key attr value)
  (setq key (nth (atoi (get_tile "key_name")) keys))
  (setq attr (nth (atoi (get_tile "attr_name")) attrs))
  (setq value (get_attr key attr))
  (set_tile "attr_value" value))

(action_tile "get_attr" "(get_attr_value)")
