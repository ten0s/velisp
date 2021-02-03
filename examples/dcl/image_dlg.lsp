(defun seq (a b / aux)
  (defun aux (a b acc)
    (if (= a b) (cons b acc)
      (aux a (1- b) (cons b acc))))
  (aux a b '()))

(setq width (dimx_tile "image1"))
(setq height (dimy_tile "image1"))

(setq cols 16)
(setq rows 16)

(setq w (fix (/ (float width) cols)))
(setq h (fix (/ (float height) rows)))

(setq dx 7)
(setq dy 5)

(start_image "image1")

(foreach color (seq 0 255)
  (setq col (rem color cols))
  (setq row (fix (/ (float color) rows)))
  (fill_image (+ (* col w) dx) (+ (* row h) dy) w h color))

(vector_image 0 0 width height 0)
(vector_image width 0 0 height 0)

(end_image)
