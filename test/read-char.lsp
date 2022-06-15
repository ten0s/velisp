(princ "Test #1\n")
(setq c1 (chr (read-char))) ; H
(setq c2 (chr (read-char))) ; e
(setq c3 (chr (read-char))) ; l
(setq c4 (chr (read-char))) ; l
(setq c5 (chr (read-char))) ; o
(setq c6 (chr (read-char))) ; \n
(setq c7 (chr (read-char))) ; \r
(princ (strcat "<|" c1 c2 c3 c4 c5 c6 c7 "|>"))

(princ "Test #2\n")
(princ (strcat "<|" (read-line) "|>")) ; Hello
(setq c1 (chr (read-char))) ; W
(setq c2 (chr (read-char))) ; o
(setq c3 (chr (read-char))) ; r
(setq c4 (chr (read-char))) ; l
(setq c5 (chr (read-char))) ; d
(setq c6 (chr (read-char))) ; \r
(princ (strcat "<|" c1 c2 c3 c4 c5 c6 "|>"))
