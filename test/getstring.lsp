(princ "Test #1\n")
(princ (strcat "<|" (getstring) "|>\n"))

(princ "Test #2\n")
(princ (strcat "<|" (getstring) "|>\n"))
(princ (strcat "<|" (getstring) "|>\n"))

(princ "Test #3\n")
(princ (strcat "<|" (getstring) "|>\n"))
(princ (strcat "<|" (getstring) "|>\n"))

(princ "Test #4\n")
(princ (strcat "<|" (getstring "SP and CR: ") "|>\n"))
(princ (strcat "<|" (getstring) "|>\n"))

(princ "Test #5\n")
(princ (strcat "<|" (getstring nil "SP and CR: ") "|>\n"))
(princ (strcat "<|" (getstring) "|>\n"))

(princ "Test #6\n")
(princ (strcat "<|" (getstring T "CR: ") "|>\n"))
