(princ "Test #1\n")
(princ (strcat "<|" (vl-princ-to-string (getint "Press return: ")) "|>\n"))

(princ "Test #2\n")
(princ (strcat "<|" (vl-princ-to-string (getint "Enter int: ")) "|>\n"))

(princ "Test #3\n")
(princ (strcat "<|" (vl-princ-to-string (getint)) "|>\n"))
