;;;;
;;;; Defines
;;;;
(setq ROWS 4 COLS 4)

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

;;;;
;;;; Graphics
;;;;

(setq BLACK_COLOR       0)
(setq RED_COLOR         1)
(setq BLUE_COLOR        5)
(setq WHITE_COLOR       255)
(setq LIGHT_GREY_COLOR  254)
(setq MIDDLE_GREY_COLOR 253)
(setq DARK_GREY_COLOR   252)

(setq ZERO_GLYPH
      '((1 0 7 0)
        (0 1 8 1)
        (0 2 1 2) (7 2 8 2)
        (0 3 1 3) (7 3 8 3)
        (0 4 1 4) (7 4 8 4)
        (0 5 1 5) (7 5 8 5)
        (0 6 1 6) (7 6 8 6)
        (0 7 1 7) (7 7 8 7)
        (0 8 1 8) (7 8 8 8)
        (0 9 1 9) (7 9 8 9)
        (0 10 1 10) (7 10 8 10)
        (0 11 1 11) (7 11 8 11)
        (0 12 8 12)
        (1 13 7 13)))

(setq ONE_GLYPH
      '((1 0 3 0)
        (0 1 3 1)
        (2 2 3 2)
        (2 3 3 3)
        (2 4 3 4)
        (2 5 3 5)
        (2 6 3 6)
        (2 7 3 7)
        (2 8 3 8)
        (2 9 3 9)
        (2 10 3 10)
        (2 11 3 11)
        (0 12 5 12)
        (0 13 5 13)))

(setq TWO_GLYPH
      '((1 0 7 0)
        (0 1 8 1)
        (0 2 1 2) (7 2 8 2)
        (7 3 8 3)
        (7 4 8 4)
        (6 5 8 5)
        (5 6 7 6)
        (4 7 6 7)
        (3 8 5 8)
        (2 9 4 9)
        (1 10 3 10)
        (0 11 2 11)
        (0 12 8 12)
        (0 13 8 13)))

(setq THREE_GLYPH
      '((0 0 8 0)
        (0 1 8 1)
        (6 2 8 2)
        (5 3 7 3)
        (4 4 6 4)
        (3 5 5 5)
        (2 6 7 6)
        (2 7 8 7)
        (7 8 8 8)
        (7 9 8 9)
        (7 10 8 10)
        (0 11 1 11) (7 11 8 11)
        (0 12 8 12)
        (1 13 7 13)))

(setq FOUR_GLYPH
      '((5 0 7 0)
        (4 1 7 1)
        (3 2 4 2) (6 2 7 2)
        (3 3 4 3) (6 3 7 3)
        (2 4 3 4) (6 4 7 4)
        (2 5 3 5) (6 5 7 5)
        (1 6 2 6) (6 6 7 6)
        (1 7 2 7) (6 7 7 7)
        (0 8 1 8) (6 8 7 8)
        (0 9 9 9)
        (0 10 9 10)
        (6 11 7 11)
        (6 12 7 12)
        (6 13 7 13)))

(setq FIVE_GLYPH
      '((0 0 8 0)
        (0 1 8 1)
        (0 2 1 2)
        (0 3 1 3)
        (0 4 1 4)
        (0 5 1 5)
        (0 6 7 6)
        (1 7 8 7)
        (7 8 8 8)
        (7 9 8 9)
        (7 10 8 10)
        (0 11 1 11) (7 11 8 11)
        (0 12 8 12)
        (1 13 7 13)))

(setq SIX_GLYPH
      '((1 0 7 0)
        (0 1 8 1)
        (0 2 1 2) (7 2 8 2)
        (0 3 1 3)
        (0 4 1 4)
        (0 5 1 5)
        (0 6 7 6)
        (0 7 1 7) (1 7 8 7)
        (0 8 1 8) (7 8 8 8)
        (0 9 1 9) (7 9 8 9)
        (0 10 1 10) (7 10 8 10)
        (0 11 1 11) (7 11 8 11)
        (0 12 8 12)
        (1 13 7 13)))

(setq SEVEN_GLYPH
      '((0 0 8 0)
        (0 1 8 1)
        (7 2 8 2)
        (6 3 7 3)
        (6 4 7 4)
        (5 5 6 5)
        (5 6 6 6)
        (4 7 5 7)
        (4 8 5 8)
        (3 9 4 9)
        (3 10 4 10)
        (2 11 3 11)
        (2 12 3 12)
        (2 13 3 13)))

(setq EIGHT_GLYPH
      '((1 0 7 0)
        (0 1 8 1)
        (0 2 1 2) (7 2 8 2)
        (0 3 1 3) (7 3 8 3)
        (0 4 1 4) (7 4 8 4)
        (0 5 1 5) (7 5 8 5)
        (1 6 7 6)
        (1 7 7 7)
        (0 8 1 8) (7 8 8 8)
        (0 9 1 9) (7 9 8 9)
        (0 10 1 10) (7 10 8 10)
        (0 11 1 11) (7 11 8 11)
        (0 12 8 12)
        (1 13 7 13)))

(setq NINE_GLYPH
      '((1 0 7 0)
        (0 1 8 1)
        (0 2 1 2) (7 2 8 2)
        (0 3 1 3) (7 3 8 3)
        (0 4 1 4) (7 4 8 4)
        (0 5 1 5) (7 5 8 5)
        (0 6 7 6) (7 6 8 6)
        (1 7 7 7) (7 7 8 7)
        (7 8 8 8)
        (7 9 8 9)
        (7 10 8 10)
        (0 11 1 11) (7 11 8 11)
        (0 12 8 12)
        (1 13 7 13)))

(defun glyph_max (glyph fun1 fun2)
  (apply 'max
         (mapcar '(lambda (lst / x1 x2)
                    (setq x1 (fun1 lst)
                          x2 (fun2 lst))
                    (max x1 x2))
                 glyph)))

(defun glyph_width (glyph)
  (glyph_max glyph car caddr))

(defun glyph_height (glyph)
  (glyph_max glyph cadr cadddr))

(defun glyph_move (glyph dx dy)
  (mapcar '(lambda (lst / x1 y1 x2 y2)
             (setq x1 (car lst)
                   y1 (cadr lst)
                   x2 (caddr lst)
                   y2 (cadddr lst))
             (list (+ x1 dx)
                   (+ y1 dy)
                   (+ x2 dx)
                   (+ y2 dy)))
          glyph))

(defun glyphs_beside (glyph1 glyph2 dx dy)
  (setq dx (+ (glyph_width glyph1) dx)
        glyph2 (glyph_move glyph2 dx dy))
  (append glyph1 glyph2))

(defun center (outer inner)
  (fix (/ (- outer inner) 2)))

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

(defun draw_glyph (key glyph color / tw th gw gh x0 y0)
  (setq tw (dimx_tile key)
        th (dimy_tile key)
        gw (glyph_width glyph)
        gh (glyph_height glyph)
        x0 (center tw gw)
        y0 (center th gh))
  (mapcar '(lambda (lst / x1 y1 x2 y2)
             (setq x1 (car lst)
                   y1 (cadr lst)
                   x2 (caddr lst)
                   y2 (cadddr lst))
             (vector_image (+ x0 x1)
                           (+ y0 y1)
                           (+ x0 x2)
                           (+ y0 y2) color))
   glyph))

(defun draw_empty (key)
  (with_image
   key
   (lambda ()
     (draw_open key))))

(defun draw_1 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key ONE_GLYPH BLACK_COLOR))))

(defun draw_2 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key TWO_GLYPH BLACK_COLOR))))

(defun draw_3 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key THREE_GLYPH BLACK_COLOR))))

(defun draw_4 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key FOUR_GLYPH BLACK_COLOR))))

(defun draw_5 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key FIVE_GLYPH BLACK_COLOR))))

(defun draw_6 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key SIX_GLYPH BLACK_COLOR))))

(defun draw_7 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key SEVEN_GLYPH BLACK_COLOR))))

(defun draw_8 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key EIGHT_GLYPH BLACK_COLOR))))

(defun draw_9 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key NINE_GLYPH BLACK_COLOR))))

(defun draw_10 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key
                 (glyphs_beside ONE_GLYPH ZERO_GLYPH 3 0)
                 BLACK_COLOR))))

(defun draw_11 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key
                 (glyphs_beside ONE_GLYPH ONE_GLYPH 3 0)
                 BLACK_COLOR))))

(defun draw_12 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key
                 (glyphs_beside ONE_GLYPH TWO_GLYPH 3 0)
                 BLACK_COLOR))))

(defun draw_13 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key
                 (glyphs_beside ONE_GLYPH THREE_GLYPH 3 0)
                 BLACK_COLOR))))

(defun draw_14 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key
                 (glyphs_beside ONE_GLYPH FOUR_GLYPH 3 0)
                 BLACK_COLOR))))

(defun draw_15 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key
                 (glyphs_beside ONE_GLYPH FIVE_GLYPH 3 0)
                 BLACK_COLOR))))

;;;;
;;;; Utilities
;;;;

;;;;
;;;; Main Logic
;;;;

;;; Global states

;;;;
;;;; DCL Dialog
;;;;

(setq dcl_file "fifteen.dcl")
(setq dlg_id "fifteen")

(if (< (setq dcl_id (load_dialog dcl_file)) 0)
  (progn
    (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
    (exit 1)))

(if (not (new_dialog dlg_id dcl_id))
  (progn
    (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
    (exit 1)))

;;;(action_tile "accept" "(done_dialog 1)")

(draw_1 "1x1")
(draw_2 "1x2")
(draw_3 "1x3")
(draw_4 "1x4")

(draw_5 "2x1")
(draw_6 "2x2")
(draw_7 "2x3")
(draw_8 "2x4")

(draw_9 "3x1")
(draw_10 "3x2")
(draw_11 "3x3")
(draw_12 "3x4")

(draw_13 "4x1")
(draw_14 "4x2")
(draw_15 "4x3")
(draw_empty "4x4")

(setq ret (start_dialog))
(princ (strcat "dialog done result: " (itoa ret) "\n"))
(unload_dialog dcl_id)
