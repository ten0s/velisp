;;;; SPDX-License-Identifier: 0BSD

(load "util.lsp")

;;;;
;;;; Globals
;;;;
(setq ROWS 8 COLS 8 MINES 8)

;;;;
;;;; VeLisp functions missing in AutoCAD
;;;;

(if (is_autocad)
    (progn
      (load "../lib/dcl/colors.lsp")
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

(load "glyph.lsp")
(load "image-button.lsp")

(setq GLYPH_FLAG
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

(setq GLYPH_QUESTION
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

(setq GLYPH_MINE
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

(defun draw_empty (key)
  (with_image
   key
   (lambda ()
     (draw_open key))))

(defun draw_1 (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_1 COLOR_BLACK))))

(defun draw_2 (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_2 COLOR_BLACK))))

(defun draw_3 (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_3 COLOR_BLACK))))

(defun draw_4 (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_4 COLOR_BLACK))))

(defun draw_5 (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_5 COLOR_BLACK))))

(defun draw_6 (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_6 COLOR_BLACK))))

(defun draw_7 (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_7 COLOR_BLACK))))

(defun draw_8 (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_8 COLOR_BLACK))))

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
     (draw_glyph key GLYPH_FLAG COLOR_BLACK))))

(defun draw_question (key)
  (with_image
   key
   (lambda ()
     (draw_closed key)
     (draw_glyph key GLYPH_QUESTION COLOR_BLACK))))

(defun draw_found_mine (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_MINE COLOR_BLUE))))

(defun draw_missed_mine (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_MINE COLOR_BLACK))))

(defun draw_exploded_mine (key)
  (with_image
   key
   (lambda ()
     (draw_open key)
     (draw_glyph key GLYPH_MINE COLOR_RED))))

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
  (setq seed (getenv "RAND_SEED"))
  (if seed (setq seed (atoi seed))
    (setq seed (srand (getvar "MILLISECS"))))
  (println (list "Rand seed: " seed))

  (reset_states)
  (init_shown_states)
  (draw_all_cells)

  (with_image
   "score_flag"
   (lambda ()
     (draw_glyph key GLYPH_FLAG COLOR_BLACK)))
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
     (draw_glyph key GLYPH_FLAG COLOR_BLUE)))
  (show_score "You Won!")
  (show_hint "Start over or exit"))

(defun game_lost ()
  (calc_final_shown_states)
  (draw_all_cells)
  (with_image
   "score_flag"
   (lambda ()
     (draw_glyph key GLYPH_FLAG COLOR_RED)))
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
        ((= state 1)              draw_1)
        ((= state 2)              draw_2)
        ((= state 3)              draw_3)
        ((= state 4)              draw_4)
        ((= state 5)              draw_5)
        ((= state 6)              draw_6)
        ((= state 7)              draw_7)
        ((= state 8)              draw_8)
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

(with_dialog
 "mines.dcl" "mines" "(button_handler $KEY $REASON)"
 (lambda ()
   (action_tile "new_game" "(start_game)")
   (set_tile "difficulty" (itoa MINES))
   (action_tile "difficulty" "(change_difficulty)")
   (start_game))
 nil)
