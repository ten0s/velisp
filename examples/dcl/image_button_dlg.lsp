(setq counter 0)
(action_tile "imagebutton1" "(setq counter (1+ counter)) (set_tile \"text1\" (itoa counter))")

(setq width (dimx "imagebutton1"))
(setq height (dimy "imagebutton1"))

(start_image "imagebutton1")
(fill_image 0 0 width height 1)
(vector_image 0 0 width height 0)
(vector_image width 0 0 height 0)
(end_image)
