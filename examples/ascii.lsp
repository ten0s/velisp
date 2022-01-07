;;;;
;;;; Simplified version of
;;;; https://documentation.help/AutoCAD-ALISP-VLISP/WS73099cc142f4875516d84be10ebc87a53f-79d8.htm
;;;;

;;; BASE converts from a decimal integer to a string in another base.
(defun BASE (bas int / ret yyy zot)
  (defun zot (i1 i2 / xxx)
    (if (> (setq xxx (rem i2 i1)) 9)
      (chr (+ 55 xxx))
      (itoa xxx)
    )
  )

  (setq ret (zot bas int)
        yyy (/ int bas)
  )
  (setq ret (strcat (zot bas yyy) ret))
  (setq yyy (/ yyy bas))

  (strcat (zot bas yyy) ret)
)

(defun C:ASCII ( / file chk code ct dec oct hex)
  (setq file (open "ascii.txt" "w")
        chk  1
        code 0
        ct   0)
  (princ "\n \n CHAR DEC OCT HEX \n")
  (princ "\n \n CHAR DEC OCT HEX \n" file)
  (while chk
    (setq dec (strcat "  " (itoa code))
          oct (base 8 code)
          hex (base 16 code))
    (setq dec (substr dec (- (strlen dec) 2) 3))
    (if (< (strlen oct) 3)
        (setq oct (strcat "0" oct)))
    
    (princ (strcat "\n " (chr code) " " dec " " oct " " hex))
    (princ (strcat "\n " (chr code) " " dec " " oct " " hex) file)

    (cond
     ((= code 255) (setq chk nil))
     ((= ct 20)
      (setq ct 0)
      (princ "\n \n CHAR DEC OCT HEX \n")))
    
    (setq ct   (1+ ct)
          code (1+ code)))

  (princ "\n")
  (princ "\n" file)
  (close file)

  (princ))

(C:ASCII)
