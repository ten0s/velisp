;;;; SPDX-License-Identifier: 0BSD

(load "util.lsp")

;;;;
;;;; Globals
;;;;

(setq *ZERO*   "0")
(setq *PERIOD* ".")
(setq *MINUS*  "-")
(setq *ERR*    "Error")

(setq *LHS* nil)
(setq *OPER* nil)

;;;;
;;;; Utilities
;;;;

;;; Define ftoa function if not defined
(if (not ftoa)
    (defun ftoa (num)
      (rtos num 2)))

(defun drop-last (lst)
  (reverse
   (cdr
    (reverse lst))))

(defun string-contains (char str)
  (/= (vl-string-position (ascii char) str) nil))

(defun string-drop-last (str)
  (vl-list->string
   (drop-last
    (vl-string->list str))))

(defun string-drop-first (str)
  (vl-list->string
   (cdr
    (vl-string->list str))))

(defun compact-float (num / int)
  (setq int (fix num))
  (if (equal int num) int num))

;;;;
;;;; DCL Helpers
;;;;

(defun get_input ( / input)
  (setq input (get_tile "input"))
  (if (= input *ERR*)
      *ZERO*
    input))

(defun set_input (input)
  (set_tile "input" input))

(defun with_input (func)
  (set_input (func (get_input))))

;;;;
;;;; Action Handlers
;;;;

(defun do_clear ()
  (with_input (lambda (_) *ZERO*)))

(defun do_backspace ()
  (with_input
   (lambda (input)
     (setq input (string-drop-last input))
     (if (= (strlen input) 0)
         *ZERO*
       input))))

(defun do_input_digit (digit)
  (with_input
   (lambda (input)
     (setq input (if (= input *ZERO*) "" input))
     (strcat input digit))))

(defun do_input_period ()
  (with_input
   (lambda (input)
     (if (string-contains *PERIOD* input)
         input
       (strcat input *PERIOD*)))))

(defun do_divide ()
  (with_input
   (lambda (input)
     (setq *LHS* input)
     (setq *OPER* /)
     *ZERO*)))

(defun do_multiply ()
  (with_input
   (lambda (input)
     (setq *LHS* input)
     (setq *OPER* *)
     *ZERO*)))

(defun do_subtract ()
  (with_input
   (lambda (input)
     (setq *LHS* input)
     (setq *OPER* -)
     *ZERO*)))

(defun do_add ()
  (with_input
   (lambda (input)
     (setq *LHS* input)
     (setq *OPER* +)
     *ZERO*)))

(defun do_negate ()
  (with_input
   (lambda (input)
     (cond ((= input *ZERO*) input)
           ((string-contains *MINUS* input) (string-drop-first input))
           (T (strcat *MINUS* input))))))

(defun do_sqrt ()
  (with_input
   (lambda (input)
     (setq input (atof input))
     (if (< input 0)
         *ERR*
       (ftoa
        (compact-float
         (sqrt input)))))))

(defun do_equal ( / lhs rhs oper)
  (with_input
   (lambda (input)
     (if (or (not *LHS*) (not *OPER*))
         (progn
           (setq *LHS* nil)
           (setq *OPER* nil)
           input)
       (progn
        (setq lhs (atof *LHS*))
        (setq rhs (atof input))
        (setq oper *OPER*)
        (setq *LHS* nil)
        (setq *OPER* nil)
        (if (and (= oper /) (= rhs 0))
            *ERR*
          (ftoa
           (compact-float
            (oper lhs rhs)))))))))
;;;;
;;;; DCL Dialog
;;;;

(with_dialog
 "calc.dcl" "calc_dlg" ""
 (lambda ()
   (action_tile "zero"   "(do_input_digit \"0\")")
   (action_tile "one"    "(do_input_digit \"1\")")
   (action_tile "two"    "(do_input_digit \"2\")")
   (action_tile "three"  "(do_input_digit \"3\")")
   (action_tile "four"   "(do_input_digit \"4\")")
   (action_tile "five"   "(do_input_digit \"5\")")
   (action_tile "six"    "(do_input_digit \"6\")")
   (action_tile "seven"  "(do_input_digit \"7\")")
   (action_tile "eight"  "(do_input_digit \"8\")")
   (action_tile "nine"   "(do_input_digit \"9\")")
   (action_tile "period" "(do_input_period)")

   (action_tile "clear"     "(do_clear)")
   (action_tile "backspace" "(do_backspace)")
   (action_tile "divide"    "(do_divide)")
   (action_tile "multiply"  "(do_multiply)")
   (action_tile "add"       "(do_add)")
   (action_tile "subtract"  "(do_subtract)")
   (action_tile "sqrt"      "(do_sqrt)")
   (action_tile "negate"    "(do_negate)")
   (action_tile "equal"     "(do_equal)"))
 nil)
