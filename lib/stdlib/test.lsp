(foreach name '("list" "vl-list" "vl-string")
         (load (strcat (getenv "VELISP_ROOT") "/lib/stdlib/" name "-test.lsp")))
