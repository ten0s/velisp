;;;;
;;;; Defines
;;;;
(setq ROWS 4 COLS ROWS)
(setq BLANK (* ROWS COLS))

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

;;;;
;;;; Binary Search Tree
;;;;

(load "bst.lsp")

;;;;
;;;; Graphics
;;;;

(setq BLACK_COLOR       0)
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

(defun draw_blank (key)
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

(defun seq (a b / aux)
  (defun aux (a b acc)
    (if (= a b) (cons b acc)
      (aux a (1- b) (cons b acc))))
  (aux a b '()))

(defun make_key (i j)
  (strcat (itoa i) "x" (itoa j)))

(defun parse_key (key / lst i j)
  (setq lst (split "x" key)
        i (car lst)
        j (cadr lst))
  (cons (atoi i) (atoi j)))

(defun make_keys (rows cols / keys)
  (foreach i (seq 1 rows)
    (foreach j (seq 1 cols)
      (setq keys (cons (make_key i j) keys))))
  (reverse keys))

(defun println (lst)
  (foreach str lst (princ str))
  (princ "\n"))

(defun inspect (msg val)
  (println (list msg val))
  val)

(defun filter (fun lst / acc)
  (foreach elm lst
           (if (fun elm)
               (setq acc (cons elm acc))))
  (reverse acc))

(defun evenp (num)
  (and (= (type num) 'INT)
       (= (rem num 2) 0)))

(defun oddp (num)
  (and (= (type num) 'INT)
       (/= (rem num 2) 0)))

;;;;
;;;; Main Logic
;;;;

;;; Global state
(setq game_state nil)

(defun win_positions ()
  (append (seq 1 (1- (* ROWS COLS))) (list BLANK)))

(defun win_state ()
  (mapcar 'cons
          (make_keys COLS ROWS)
          (win_positions)))

(defun gen_state ()
  (mapcar 'cons
          (make_keys COLS ROWS)
          (shuffle (win_positions))))

;;; Determining solvability
;;; https://web.archive.org/web/20201009161204/http://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
;;; If the grid width is odd, then the number of inversions in a solvable situation is even
;;; If the grid width is even, and the blank is on an even row counting from the bottom
;;; (second-last, fourth-last etc), then the number of inversions in a solvable situation is odd
;;; If the grid width is even, and the blank is on an odd row counting from the bottom
;;; (last, third-last, fifth-last etc) then the number of inversions in a solvable situation is even

(defun inversions (state / aux)
  (defun aux (xs / x)
    (if (null xs) 0
      (progn
      (setq x (car xs))
      (+ (length (filter (lambda (y) (> x y)) xs))
         (aux (cdr xs))))))
  (aux (filter (lambda (x) (/= x BLANK))
                 (mapcar 'cdr state))))

(defun get_blank_key (state)
  (caar (vl-member-if
         (lambda (key_pos) (= (cdr key_pos) BLANK)) state)))

(defun is_solvable (state / invers_num blank_y blank_row)
  (setq invers_num (inversions state)
        blank_y    (1- (car (parse_key (get_blank_key state))))
        blank_row  (- ROWS blank_y))
  ;(println (list invers_num " " blank_y " " blank_row))
  (if (oddp COLS)
      (evenp invers_num)
    (= (oddp blank_row) (evenp invers_num))))

(defun init_state ( / state)
  (setq state (gen_state))
  (while (not (is_solvable state))
    (setq state (gen_state)))
  (setq game_state (bst_from_list state)))

(defun print_state ()
  (println (list "State:\n" (bst_to_list game_state))))

(defun reset_state ()
  (setq game_state nil))

(defun get_state (key)
  (bst_get key game_state))

(defun set_state (key state)
  (setq game_state (bst_set key state game_state)))

(defun is_game_started ()
  (/= game_state nil))

(defun is_blank_tile (key)
  (= (get_state key) 0))

(defun show_hint (message)
  (set_tile "error" message))

(defun start_game ()
  (setq seed (srand (getvar "MILLISECS")))
  (println (list "Rand seed: " seed))

  (reset_state)
  (init_state)
  (print_state)
  (draw_all_tiles)

  (show_hint (strcat "Move tiles to order them from 1 to 15, then\n"
                     "the blank. To move a tile click on it")))

(defun check_game_over ()
  (if (equal (bst_to_list game_state) (win_state))
      (game_over)))

(defun game_over ()
  (reset_state)
  (show_hint (strcat "Well done!\n"
                     "Start over or exit")))

(defun state_to_draw_fun (state)
  (cond ((= state 1)     draw_1)
        ((= state 2)     draw_2)
        ((= state 3)     draw_3)
        ((= state 4)     draw_4)
        ((= state 5)     draw_5)
        ((= state 6)     draw_6)
        ((= state 7)     draw_7)
        ((= state 8)     draw_8)
        ((= state 9)     draw_9)
        ((= state 10)    draw_10)
        ((= state 11)    draw_11)
        ((= state 12)    draw_12)
        ((= state 13)    draw_13)
        ((= state 14)    draw_14)
        ((= state 15)    draw_15)
        ((= state BLANK) draw_blank)
        (T (println (list "Unknown state: " state)))))

(defun draw_tile (key state / draw_fun)
  (setq draw_fun (state_to_draw_fun state))
  (draw_fun key))

(defun draw_all_tiles ()
  (foreach key (make_keys ROWS COLS)
           (draw_tile key (get_state key))))

(defun fold_around_tiles (key fun init / acc coord i j k l)
  (setq acc init
        coord (parse_key key)
        i (car coord)
        j (cdr coord))
  (foreach d '(;(-1 . -1) diagonal
               ;( 1 . -1) diagonal
               ;(-1 .  1) diagonal
               ;( 1 .  1) diagonal
               ( 0 . -1)
               (-1 .  0)
               ( 1 .  0)
               ( 0 .  1))
           (setq k (+ i (car d))
                 l (+ j (cdr d)))
           (if (and (>= k 1) (<= k ROWS)
                    (>= l 1) (<= l COLS))
               (setq acc (fun (make_key k l) acc))))
  acc)

;;; (key) -> key | nil
(defun nearby_blank_key (key)
  (fold_around_tiles
   key
   (lambda (around_key acc)
     (if acc acc
       (if (= (get_state around_key) BLANK) around_key
         acc)))
   nil))

;;;;
;;;; DCL Dialog
;;;;

(setq dcl_file "fifteen.dcl")
(setq dlg_id "fifteen")

(if (< (setq dcl_id (load_dialog dcl_file)) 0)
  (progn
    (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
    (exit 1)))

(if (not (new_dialog dlg_id dcl_id "(button_handler $KEY $REASON)" '(0 0)))
  (progn
    (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
    (exit 1)))

(defun button_handler (key reason)
  (if (= reason 1)
      (single_click_handler key)))

(defun single_click_handler (key / old_state new_state blank_key)
  (println (list "Click: " key))
  (if (and (is_game_started) (not (is_blank_tile key)))
      (progn
        (setq old_state (get_state key)
              blank_key (inspect "Blank: " (nearby_blank_key key)))
        (if blank_key
            (progn
              (setq new_state BLANK)
              (println (list key ": " old_state " -> " new_state))
              (println (list blank_key ": " BLANK " -> " old_state))
              (set_state key new_state)
              (set_state blank_key old_state)
              (print_state)
              (draw_tile key new_state)
              (draw_tile blank_key old_state)
              (check_game_over))))))

(action_tile "new_game" "(start_game)")

(start_game)

(start_dialog)
(unload_dialog dcl_id)
