(princ "Test #1\n")
(princ (strcat "<|" (vl-princ-to-string (getreal "Press return: ")) "|>\n"))

(princ "Test #2\n")
(princ (strcat "<|" (vl-princ-to-string (getreal "Enter real: ")) "|>\n"))

(princ "Test #3\n")
(princ (strcat "<|" (vl-princ-to-string (getreal)) "|>\n"))
