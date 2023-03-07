;;;; SPDX-License-Identifier: 0BSD

(defun with_image (key draw_fun)
  (start_image key)
  (draw_fun)
  (end_image))

(defun draw_closed (key / tw th)
  (setq tw (dimx_tile key)
        th (dimy_tile key))

  (fill_image 0 0 tw th COLOR_WHITE)
  (fill_image 4 4 (- tw 4) (- th 4) COLOR_LIGHT_GREY)

  ;; Vertical right edge
  (vector_image tw 0 tw th COLOR_MIDDLE_GREY)
  (vector_image (- tw 1) 1 (- tw 1) (- th 1) COLOR_MIDDLE_GREY)
  (vector_image (- tw 2) 2 (- tw 2) (- th 2) COLOR_MIDDLE_GREY)
  (vector_image (- tw 3) 3 (- tw 3) (- th 3) COLOR_MIDDLE_GREY)

  ;; Horizontal botton edge
  (vector_image 0 th tw th COLOR_MIDDLE_GREY)
  (vector_image 1 (- th 1) (- tw 1) (- th 1) COLOR_MIDDLE_GREY)
  (vector_image 2 (- th 2) (- tw 2) (- th 2) COLOR_MIDDLE_GREY)
  (vector_image 3 (- th 3) (- tw 3) (- th 3) COLOR_MIDDLE_GREY))

(defun draw_open (key / tw th)
  (setq tw (dimx_tile key)
        th (dimy_tile key))

  (fill_image 0 0 tw th COLOR_WHITE)

  ;; Vertical left edge
  (vector_image 0 0 0 th COLOR_LIGHT_GREY)
  (vector_image 1 1 1 (- th 1) COLOR_LIGHT_GREY)
  (vector_image 2 2 2 (- th 2) COLOR_LIGHT_GREY)
  (vector_image 3 3 3 (- th 3) COLOR_LIGHT_GREY)

  ;; Horizontal top edge
  (vector_image 0 0 tw 0 COLOR_LIGHT_GREY)
  (vector_image 1 1 (- tw 1) 1 COLOR_LIGHT_GREY)
  (vector_image 2 2 (- tw 2) 2 COLOR_LIGHT_GREY)
  (vector_image 3 3 (- tw 3) 3 COLOR_LIGHT_GREY))
