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

(defun draw_glyph (key glyph color / tw th gw gh x0 y0)
  (setq tw (dimx_tile key)
        th (dimy_tile key)
        gw (glyph_width glyph)
        gh (glyph_height glyph)
        x0 (glyph_center tw gw)
        y0 (glyph_center th gh))
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

(defun glyph_center (outer inner)
  (fix (/ (- outer inner) 2)))

(setq GLYPH_0
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

(setq GLYPH_1
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

(setq GLYPH_2
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

(setq GLYPH_3
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

(setq GLYPH_4
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

(setq GLYPH_5
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

(setq GLYPH_6
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

(setq GLYPH_7
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

(setq GLYPH_8
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

(setq GLYPH_9
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
