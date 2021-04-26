(princ "Test #1\n")
(princ (strcat "<|" (vl-princ-to-string (getreal "Press return: ")) "|>"))

(princ "Test #2\n")
(princ (strcat "<|" (vl-princ-to-string (getreal "Enter real: ")) "|>"))

(princ "Test #3\n")
(princ (strcat "<|" (vl-princ-to-string (getreal)) "|>"))
