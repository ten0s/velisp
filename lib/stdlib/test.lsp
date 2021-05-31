(foreach name '("list" "vl-filename" "vl-list" "vl-string" "ve-list" "ve-string")
         (load (strcat (getenv "VELISP_ROOT") "/lib/stdlib/" name "-test.lsp")))
