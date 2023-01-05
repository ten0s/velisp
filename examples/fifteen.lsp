;;;; SPDX-License-Identifier: 0BSD

;;;;
;;;; Defines
;;;;
(setq ROWS 4 COLS ROWS)
(setq BLANK (* ROWS COLS))

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

(if (null %VELISP_VERSION%)
    (progn
      (load "../lib/velisp/list.lsp")
      (load "../lib/velisp/random.lsp")
      (load "../lib/velisp/string.lsp")))

;;;;
;;;; Binary Search Tree
;;;;

(load "bst.lsp")

;;;;
;;;; Graphics
;;;;

(load "color.lsp")
(load "glyph.lsp")
(load "image-button.lsp")

(setq GLYPH_10
      (glyphs_beside GLYPH_1 GLYPH_0 3 0))

(setq GLYPH_11
      (glyphs_beside GLYPH_1 GLYPH_1 3 0))

(setq GLYPH_12
      (glyphs_beside GLYPH_1 GLYPH_2 3 0))

(setq GLYPH_13
      (glyphs_beside GLYPH_1 GLYPH_3 3 0))

(setq GLYPH_14
      (glyphs_beside GLYPH_1 GLYPH_4 3 0))

(setq GLYPH_15
      (glyphs_beside GLYPH_1 GLYPH_5 3 0))

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
     (draw_glyph key GLYPH_1 BLACK_COLOR))))

(defun draw_2 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_2 BLACK_COLOR))))

(defun draw_3 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_3 BLACK_COLOR))))

(defun draw_4 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_4 BLACK_COLOR))))

(defun draw_5 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_5 BLACK_COLOR))))

(defun draw_6 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_6 BLACK_COLOR))))

(defun draw_7 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_7 BLACK_COLOR))))

(defun draw_8 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_8 BLACK_COLOR))))

(defun draw_9 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_9 BLACK_COLOR))))

(defun draw_10 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_10 BLACK_COLOR))))

(defun draw_11 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_11 BLACK_COLOR))))

(defun draw_12 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_12 BLACK_COLOR))))

(defun draw_13 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_13 BLACK_COLOR))))

(defun draw_14 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_14 BLACK_COLOR))))

(defun draw_15 (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_15 BLACK_COLOR))))

;;;;
;;;; Utilities
;;;;

(defun make_key (i j)
  (strcat (itoa i) "x" (itoa j)))

(defun parse_key (key / lst i j)
  (setq lst (split "x" key)
        i (car lst)
        j (cadr lst))
  (cons (atoi i) (atoi j)))

(defun make_keys (rows cols / keys)
  (foreach i (seq 1 rows 1)
    (foreach j (seq 1 cols 1)
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
  (append
   (seq 1 (1- (* ROWS COLS)) 1)
   (list BLANK)))

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
         '(lambda (key_pos) (= (cdr key_pos) BLANK)) state)))

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
  (= (get_state key) BLANK))

(defun show_hint (message)
  (set_tile "error" message))

(defun start_game ( / seed)
  (setq seed (getenv "RAND_SEED"))
  (if seed (setq seed (atoi seed))
    (setq seed (srand (getvar "MILLISECS"))))
  (println (list "Rand seed: " seed))

  (reset_state)
  (init_state)
  (print_state)
  (draw_all_tiles)

  (show_hint (strcat "Move tiles to order them from 1 to 15")))

(defun check_game_over ()
  (if (equal (bst_to_list game_state) (win_state))
      (game_over)))

(defun game_over ()
  (reset_state)
  (show_hint (strcat "Well done! Start over or exit")))

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

(if (not (new_dialog dlg_id dcl_id "(button_handler $KEY $REASON)"))
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
