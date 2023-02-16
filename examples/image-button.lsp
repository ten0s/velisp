;;;; SPDX-License-Identifier: 0BSD

(defun with_image (key draw_fun)
  (start_image key)
  (draw_fun)
  (end_image))

(defun draw_closed (key / tw th)
  (setq tw (dimx_tile key)
        th (dimy_tile key))

  (fill_image 0 0 tw th WHITE_COLOR)
  (fill_image 4 4 (- tw 4) (- th 4) LIGHT_GREY_COLOR)

  ;; Vertical right edge
  (vector_image tw 0 tw th MIDDLE_GREY_COLOR)
  (vector_image (- tw 1) 1 (- tw 1) (- th 1) MIDDLE_GREY_COLOR)
  (vector_image (- tw 2) 2 (- tw 2) (- th 2) MIDDLE_GREY_COLOR)
  (vector_image (- tw 3) 3 (- tw 3) (- th 3) MIDDLE_GREY_COLOR)

  ;; Horizontal botton edge
  (vector_image 0 th tw th MIDDLE_GREY_COLOR)
  (vector_image 1 (- th 1) (- tw 1) (- th 1) MIDDLE_GREY_COLOR)
  (vector_image 2 (- th 2) (- tw 2) (- th 2) MIDDLE_GREY_COLOR)
  (vector_image 3 (- th 3) (- tw 3) (- th 3) MIDDLE_GREY_COLOR))

(defun draw_open (key / tw th)
  (setq tw (dimx_tile key)
        th (dimy_tile key))

  (fill_image 0 0 tw th WHITE_COLOR)

  ;; Vertical left edge
  (vector_image 0 0 0 th LIGHT_GREY_COLOR)
  (vector_image 1 1 1 (- th 1) LIGHT_GREY_COLOR)
  (vector_image 2 2 2 (- th 2) LIGHT_GREY_COLOR)
  (vector_image 3 3 3 (- th 3) LIGHT_GREY_COLOR)

  ;; Horizontal top edge
  (vector_image 0 0 tw 0 LIGHT_GREY_COLOR)
  (vector_image 1 1 (- tw 1) 1 LIGHT_GREY_COLOR)
  (vector_image 2 2 (- tw 2) 2 LIGHT_GREY_COLOR)
  (vector_image 3 3 (- tw 3) 3 LIGHT_GREY_COLOR))
