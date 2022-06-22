(setq counter 0)

(defun update_status (x y)
  (setq counter (1+ counter))
  (set_tile "counter" (itoa counter))
  (set_tile "coords" (strcat "(" (itoa x) "," (itoa y) ")")))

(action_tile "button" "(update_status $x $y)")

(setq width (dimx_tile "button"))
(setq height (dimy_tile "button"))

(start_image "button")
(fill_image 0 0 width height 1)
(vector_image 0 0 width height 0)
(vector_image width 0 0 height 0)
(end_image)
