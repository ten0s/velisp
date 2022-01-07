;;;;
;;;; Defines
;;;;
(setq ROWS 8 COLS 8 MINES 8)

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

(if (not srand)
    (defun srand (seed / m)
      ;; Initializes pseudo-random number generator
      (setq m 2147483648
            %VELISP_RAND_SEED% (rem (fix seed) m))))

(if (not rand)
    (defun rand ( / a c m d)
      ;; Returns a pseudo-random integral number in the range between 0 and 32767
      (setq a 214013
            c 2531011
            m 2147483648
            d 65536
            %VELISP_RAND_SEED% (rem (+ (* %VELISP_RAND_SEED% a) c) m))
      (fix (/ %VELISP_RAND_SEED% d))))

(if (not sort)
    (defun sort (cmp lst / insert len)
      ;; Sorts the elements in a list by the insertion sort
      ;; according to a given compare function
      (defun insert (item sorted-lst)
        (cond ((null sorted-lst) (list item))
              ((cmp item (car sorted-lst)) (cons item sorted-lst))
              (T (cons (car sorted-lst)
                       (insert item (cdr sorted-lst))))))
      (setq len (length lst))
      (cond ((= len 0) lst)
            ((= len 1) lst)
            (T (insert (car lst) (sort cmp (cdr lst)))))))

(if (not shuffle)
    (defun shuffle (lst)
      ;; Shuffles randomly the elements in a list
      (mapcar 'cdr
              (sort (lambda (l r) (< (car l) (car r)))
                    (mapcar '(lambda (x) (cons (rand) x)) lst)))))

(if (not split)
    (defun split (delim str / delim-len do-split)
      ;; Split a string using a delimiter
      (setq delim-len (strlen delim))
      (defun do-split (str / pos)
        (if (setq pos (vl-string-search delim str))
            (cons (substr str 1 pos)
                  (do-split (substr str (+ pos 1 delim-len))))
          (list str)))
      (if (zerop delim-len)
          (mapcar 'chr (vl-string->list str))
        (do-split str))))

;;;;
;;;; Binary Search Tree
;;;;

(load "bst.lsp")

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

(setq FLAG_GLYPH
      '((0 0 4 0)
        (0 1 5 1)
        (0 2 6 2)
        (0 3 11 3)
        (0 4 11 4)
        (0 5 11 5)
        (0 6 11 6)
        (0 7 1 7) (5 7 11 7)
        (0 8 1 8) (6 8 11 8)
        (0 9 1 9) (7 9 11 9)
        (0 10 1 10)
        (0 11 1 11)
        (0 12 1 12)
        (0 13 1 13)
        (0 14 1 14)
        (0 15 1 15)))

(setq QUESTION_GLYPH
      '((1 0 7 0)
        (0 1 8 1)
        (0 2 1 2) (7 2 8 2)
        (7 3 8 3)
        (7 4 8 4)
        (6 5 8 5)
        (5 6 7 6)
        (4 7 6 7)
        (4 8 5 8)
        (3 9 5 9)
        (3 10 4 10)
        (3 11 4 11)
        (3 14 4 14)
        (3 15 4 15)))

(setq MINE_GLYPH
      '((9 0 10 0)
        (9 1 10 1)
        (9 2 10 2)
        (7 3 12 3)
        (7 4 12 4)
        (5 5 14 5)
        (5 6 14 6)
        (3 7 6 7) (9 7 16 7)
        (3 8 6 8) (9 8 16 8)
        (0 9 19 9)
        (0 10 19 10)
        (3 11 16 11)
        (3 12 16 12)
        (5 13 14 13)
        (5 14 14 14)
        (7 15 12 15)
        (7 16 12 16)
        (9 17 10 17)
        (9 18 10 18)
        (9 19 10 19)))

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

(defun draw_one (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key ONE_GLYPH BLACK_COLOR))))

(defun draw_two (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key TWO_GLYPH BLACK_COLOR))))

(defun draw_three (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key THREE_GLYPH BLACK_COLOR))))

(defun draw_four (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key FOUR_GLYPH BLACK_COLOR))))

(defun draw_five (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key FIVE_GLYPH BLACK_COLOR))))

(defun draw_six (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key SIX_GLYPH BLACK_COLOR))))

(defun draw_seven (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key SEVEN_GLYPH BLACK_COLOR))))

(defun draw_eight (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key EIGHT_GLYPH BLACK_COLOR))))

(defun draw_hidden (key)
  (with_image
   key
   (lambda ()
     (draw_closed key))))

(defun draw_flag (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key FLAG_GLYPH BLACK_COLOR))))

(defun draw_question (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key QUESTION_GLYPH BLACK_COLOR))))

(defun draw_found_mine (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key MINE_GLYPH BLUE_COLOR))))

(defun draw_missed_mine (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key MINE_GLYPH BLACK_COLOR))))

(defun draw_exploded_mine (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key MINE_GLYPH RED_COLOR))))

;;;;
;;;; Utilities
;;;;

(defun seq (a b / aux)
  (defun aux (a b acc)
    (if (= a b) (cons b acc)
      (aux a (1- b) (cons b acc))))
  (aux a b '()))

(defun duplicate (n elm / lst)
  (while (> n 0)
    (setq n (1- n)
          lst (cons elm lst))))

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

;;;;
;;;; Main Logic
;;;;

;;; Global states
(setq hidden_states nil
      shown_states  nil)

(defun reset_states ()
  (setq hidden_states nil
        shown_states  nil))

(defun show_score (message)
  (set_tile "score_text" message))

(defun make_score (flags mines)
  (strcat (itoa flags) "/" (itoa mines)))

(defun update_score (flags mines)
  (show_score (make_score flags mines)))

(defun change_difficulty ()
  (setq MINES (get_difficulty))
  (show_score (make_score 0 MINES)))

(defun get_difficulty ()
  (atoi (get_tile "difficulty")))

(defun enable_difficulty ()
  (mode_tile "difficulty" 0))

(defun disable_difficulty ()
  (mode_tile "difficulty" 1))

(defun show_hint (message)
  (set_tile "error" (strcat "Hint: " message)))

(defun start_game ( / seed)
  (setq seed (srand (getvar "MILLISECS")))
  (println (list "Rand seed: " seed))

  (reset_states)
  (init_shown_states)
  (draw_all_cells)

  (with_image
   "score_flag"
   (lambda ()
     (draw_glyph key FLAG_GLYPH BLACK_COLOR)))
  (enable_difficulty)
  (setq MINES (get_difficulty))
  (show_score (make_score 0 MINES))
  (show_hint "Double-click a cell to start the game"))

(defun game_won ()
  (calc_final_shown_states)
  (draw_all_cells)
  (with_image
   "score_flag"
   (lambda ()
     (draw_glyph key FLAG_GLYPH BLUE_COLOR)))
  (show_score "You Won!")
  (show_hint "Start over or exit"))

(defun game_lost ()
  (calc_final_shown_states)
  (draw_all_cells)
  (with_image
   "score_flag"
   (lambda ()
     (draw_glyph key FLAG_GLYPH RED_COLOR)))
  (show_score "You Lost!")
  (show_hint "Start over or exit"))

(defun check_game_won ( / all flags_and_opens flags opens)
  (setq all (* ROWS COLS)
        flags_and_opens (calc_flag_and_open_cells)
        flags (car flags_and_opens)
        opens (cdr flags_and_opens))
  (update_score flags MINES)
  (if (= (- all flags opens) 0)
      (game_won)))

(defun is_game_started ()
  (/= hidden_states nil))

(defun is_cell_closed (key)
  (member (get_shown_state key)
          '(HIDDEN FLAG QUESTION)))

;; () -> ((key . MINE | NONE))
(defun gen_hidden_states ()
  (mapcar '(lambda (key state) (cons key state))
          (make_keys ROWS COLS)
          (shuffle (append
                    (duplicate (- (* ROWS COLS) MINES) 'NONE)
                    (duplicate MINES 'MINE)))))

;; () -> (key . HIDDEN)
(defun gen_shown_states ()
  (mapcar '(lambda (key) (cons key 'HIDDEN))
          (make_keys ROWS COLS)))

(defun init_hidden_states ()
  (setq hidden_states
        (bst_from_list (gen_hidden_states)))
  (println (list "Hidden states:\n" hidden_states)))

(defun calc_mines_around_hidden_states ()
  (setq hidden_states
        (bst_map (lambda (key state)
                   (if (= state 'NONE)
                       (mines_around key)
                     state))
                 hidden_states))
  (println (list "Hidden states:\n" hidden_states)))

(defun init_shown_states ()
  (setq shown_states
        (bst_from_list (gen_shown_states)))
  (println (list "Shown states:\n" shown_states)))

(defun calc_final_shown_states ()
  (setq shown_states
        (bst_map (lambda (key shown_state / hidden_state)
                   (if (is_cell_closed key)
                       (progn
                         (setq hidden_state (get_hidden_state key))
                         (if (= hidden_state 'MINE)
                             (if (= shown_state 'FLAG)
                                 'FOUND_MINE
                               'MISSED_MINE)
                           hidden_state))
                     shown_state))
                shown_states))
  (println (list "Shown states:\n" shown_states)))

(defun get_hidden_state (key)
  (bst_get key hidden_states))

(defun get_shown_state (key)
  (bst_get key shown_states))

(defun set_shown_state (key state)
  (setq shown_states (bst_set key state shown_states))
  (println (list "Shown states:\n" shown_states)))

(defun next_shown_state (state)
  (if (not (is_game_started))
      'HIDDEN
    (cond ((= state 'HIDDEN)   'FLAG)
          ((= state 'FLAG)     'QUESTION)
          ((= state 'QUESTION) 'HIDDEN)
          (T state))))

(defun shown_state_to_draw_fun (state)
  (cond ((= state 'HIDDEN)        draw_hidden)
        ((= state 'FLAG)          draw_flag)
        ((= state 'QUESTION)      draw_question)
        ((= state 'FOUND_MINE)    draw_found_mine)
        ((= state 'MISSED_MINE)   draw_missed_mine)
        ((= state 'EXPLODED_MINE) draw_exploded_mine)
        ((= state 0)              draw_empty)
        ((= state 1)              draw_one)
        ((= state 2)              draw_two)
        ((= state 3)              draw_three)
        ((= state 4)              draw_four)
        ((= state 5)              draw_five)
        ((= state 6)              draw_six)
        ((= state 7)              draw_seven)
        ((= state 8)              draw_eight)
        (T (println (list "Unknown state: " state)))))

(defun draw_cell (key state / draw_fun)
  (setq draw_fun (shown_state_to_draw_fun state))
  (draw_fun key))

(defun draw_all_cells ()
  (foreach key (make_keys ROWS COLS)
           (draw_cell key (get_shown_state key))))

;;; () -> (flags . opens)
(defun calc_flag_and_open_cells ( / flags opens)
  (setq flags 0 opens 0)
  (foreach key (make_keys ROWS COLS)
           (if (= (get_shown_state key) 'FLAG)
               (setq flags (1+ flags)))
           (if (not (is_cell_closed key))
               (setq opens (1+ opens))))
  (cons flags opens))

(defun open_cell (key / hidden_state old_state new_state)
  (setq hidden_state (get_hidden_state key))
  (if (= hidden_state 'MINE)
      (progn
        (setq old_state (get_shown_state key)
              new_state 'EXPLODED_MINE)
        (println (list old_state " -> " new_state))
        (set_shown_state key new_state)
        (draw_cell key new_state)
        (game_lost))
    (progn
      (setq old_state (get_shown_state key)
            new_state hidden_state)
      (println (list old_state " -> " new_state))
      (set_shown_state key new_state)
      (draw_cell key new_state)
      (if (= new_state 0) ; if no mines around
          (open_around_cells key))
      (check_game_won))))

(defun fold_around_cells (key fun init / acc coord i j k l)
  (setq acc init
        coord (parse_key key)
        i (car coord)
        j (cdr coord))
  (foreach d '((-1 . -1)
               ( 0 . -1)
               ( 1 . -1)
               (-1 .  0)
               ( 1 .  0)
               (-1 .  1)
               ( 0 .  1)
               ( 1 .  1))
           (setq k (+ i (car d))
                 l (+ j (cdr d)))
           (if (and (>= k 1) (<= k ROWS)
                    (>= l 1) (<= l COLS))
               (setq acc (fun (make_key k l) acc))))
  acc)

(defun mines_around (key)
  (fold_around_cells
   key
   (lambda (around_key acc)
     (if (= (get_hidden_state around_key) 'MINE) (1+ acc) acc))
     0))

(defun open_around_cells (key / old_state new_state)
  (fold_around_cells
   key
   (lambda (around_key _acc)
     (if (is_cell_closed around_key)
         (progn
           (setq old_state (get_shown_state around_key)
                 new_state (get_hidden_state around_key))
           (if (not (null new_state))
               (progn
                 (println (list old_state " -> " new_state))
                 (set_shown_state around_key new_state)
                 (draw_cell around_key new_state)
                 (if (= new_state 0) ; if no mines around
                     (open_around_cells around_key)))))))
   'ignored))

;;;;
;;;; DCL Dialog
;;;;

(setq dcl_file "mines.dcl")
(setq dlg_id "mines")

(if (< (setq dcl_id (load_dialog dcl_file)) 0)
  (progn
    (princ (strcat "Error: dcl file '" dcl_file "' not loaded\n"))
    (exit 1)))

(if (not (new_dialog dlg_id dcl_id "(button_handler $KEY $REASON)" '(0 0)))
  (progn
    (princ (strcat "Error: dialog '" dlg_id "' not found\n"))
    (exit 1)))

(action_tile "new_game" "(start_game)")
(set_tile "difficulty" (itoa MINES))
(action_tile "difficulty" "(change_difficulty)")

(defun button_handler (key reason)
  (cond ((= reason 1) (single_click_handler key))
        ((= reason 4) (double_click_handler key))
        (T nil)))

(defun single_click_handler (key / old_state new_state)
  (println (list "Single click: " key))
  (if (and (is_game_started) (is_cell_closed key))
      (progn
        (setq old_state (get_shown_state key)
              new_state (next_shown_state old_state))
        (println (list old_state " -> " new_state))
        (set_shown_state key new_state)
        (draw_cell key new_state)
        (check_game_won))))

(defun double_click_handler (key)
  (println (list "Double click: " key))
  (if (not (is_game_started))
      ;; Generate hidden state such that first
      ;; double clicked cell is NOT mined
      (progn
        (disable_difficulty)
        (init_hidden_states)
        (while (= (get_hidden_state key) 'MINE)
          (init_hidden_states))
        (calc_mines_around_hidden_states)
        (show_hint "Single-click to flag or question, double-click to open"))
    )
  (if (is_cell_closed key)
      (open_cell key)))

(start_game)

(start_dialog)
(unload_dialog dcl_id)
