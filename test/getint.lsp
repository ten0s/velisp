(princ "Test #1\n")
(princ (strcat "<|" (vl-princ-to-string (getint "Press return: ")) "|>"))

(princ "Test #2\n")
(princ (strcat "<|" (vl-princ-to-string (getint "Enter int: ")) "|>"))

(princ "Test #3\n")
(princ (strcat "<|" (vl-princ-to-string (getint)) "|>"))
